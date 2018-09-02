setClass("Dataset", representation(file="File", path="character", uuid="character",
  shape="numeric", type="list"))

#' constructor for a Dataset 
#' @name Dataset
#' @param file an object of type File
#' @param path intra-file path string
#' @export
Dataset <- function(file, path)  {

  idx <- which(file@dsetdf[,1] == path)
  if (length(idx) == 0)  
    stop("no such dataset")
  uuid <- file@dsetdf[idx,2]
  request <- paste0(file@src@endpoint, '/datasets/', uuid, '?domain=', file@domain)
  response <- submitRequest(request)

  shape <- response$shape$dims
  type <- list(class=response$type$class, base=response$type$base)

  obj <- new("Dataset", file=file, path=path, uuid=uuid,
    shape=shape, type=type)
}



#' getData basic data-fetching functionality
#' @name getData
#' @param dataset an object of type Dataset
#' @param slices array of valid character slices in R format
#' @export
getData <- function(dataset, slices, transfermode = 'JSON')  {
  slices <- checkSlices(dataset@shape, slices)
  if (length(slices) == 0)
    stop("bad slices")
  if (!(transfermode %in% c('JSON', 'binary')))  {
    warning('unrecognized transfermode, using JSON')
    transfermode <- 'JSON'
  }

  sdims <- vapply(slices, slicelen, numeric(1))
  slices <- vapply(slices, function(s) { s }, character(1))
  sel <- paste0('[', paste(slices, collapse=','), ']')
  endpoint <- dataset@file@src@endpoint
  domain <- dataset@file@domain

  request <- paste0(endpoint, '/datasets/', dataset@uuid, 
    '/value?domain=', domain, '&select=', sel)
  response <- submitRequest(request, transfermode=transfermode)

  nn <- prod(sdims)
  A <- array(rep(0, nn))
  if (transfermode == 'JSON')  {
    result <- response$value
    # unpack response into an R array 
    A[1:nn] <- vapply(1:nn, function(i) {
      idx <- csub4idx(sdims, i)
      result[[idx]]
    }, numeric(1))
  } else if (transfermode == 'binary')  {
    result <- extractBinary(dataset@type, nn, response)
    A[1:nn] <- vapply(1:nn, function(i) {
      idx <- ridx4sub(sdims, csub4idx(sdims, i)) 
      result[idx]
    }, numeric(1))
  }
  AA <- array(A, dim = sdims)

}

# private - in which we try to anticipate all the invalid things 
# users will try to enter for indices
checkSlices <- function(shape, slices)  {
  ok <- TRUE
  if (length(slices) != length(shape)) {
    message("wrong number of indices")
    ok <- FALSE
  }
  
  slicelist <- vector("list", length(slices))

  for (i in seq_along(slices))  {
    slice <- slices[i]
    start <- -1
    stop <- -1
    step <- -1

    st <- strsplit(slice, ':')[[1]]
    ss <- as.numeric(st)
    ss[which(is.na(ss))] <- -1

    if (slice == ':')  {
      start <- 1
      stop <- shape[i]
      step <- 1
    } else {
      if (length(ss) == 1)  {    # this is a slice like '5:' 
        start <- ss[1]
        stop <- shape[i]
        step <- 1
      } else if (length(ss) == 2)  {
        if (st[1] == '')  {      # ':5'
          start <- 1 
          stop <- ss[2]
          step <- 1
        } else  {
          start <- ss[1]
          stop <- ss[2]
          step <- 1
        }
      } else if (length(ss) == 3)  {
          start <- ss[1]
          stop <- ss[2]
          step <- ss[3]
      } else  {
        message(paste0("malformed slice ", i))
        ok <- FALSE
      }
    }

    slicevec <- c(start, stop, step)
    if (any(slicevec < 0))  {
      message(paste0("malformed slice ", i))
      ok <- FALSE
    }
    if (any(slicevec != trunc(slicevec)))  {
      message(paste0("malformed slice ", i))
      ok <- FALSE
    }

    slicelist[[i]] <- slicevec

    if (0 >= start || start > shape[i])  {
      message(paste0("slice start out of range in slice ", i))
      ok <- FALSE
    }
    if (0 >= stop || stop > shape[i])  {
      message(paste0("slice stop out of range in slice ", i))
      ok <- FALSE
    }
    if (stop < start)  {
      message(paste0("slice stop less than slice start in slice ", i))
      ok <- FALSE
    }
  }

  if (!ok)
    return(list())

  pyslices <- lapply(slicelist, function(slc)  {
    # python indices from 0 instead of 1
    slc[1] <- slc[1]-1
    slc[2] <- slc[2]-1
    # if (stop-start) % step == 0, python slice excludes stop
    if ( (slc[2]-slc[1]) %% slc[3] == 0 )
      slc[2] <- slc[2] + 1
    sprintf('%d:%d:%d', slc[1], slc[2], slc[3])
  })

}

# Note: for more than two dimensions, "column-major" means
# "first-fastest" and "row-major" means "last-fastest"

# private - extract column-major subscripts for linear index
csub4idx <- function(D, ind)  {
  m <- length(D)
  X <- rep(-1, m)
  off <- ind-1
  
  X <- vapply(0:(m-1), function(j)  {
    if (j == 0)  {
      s <- off %% D[1]      
    } else if (j == m-1)  {
      s <- off %/% prod(D[1:m-1])
    } else  {
      s <- (off %/% prod(D[1:j])) %% D[j+1]
    } 
    s
  }, numeric(1))
  X <- X+1
  X
}

# private - extract column-major subscripts for linear index
rsub4idx <- function(D, ind)  {
  m <- length(D)
  X <- rep(-1, m)
  off <- ind-1
  
  X <- vapply(0:(m-1), function(j)  {
    if (j == 0)  {
      s <- off %/% prod(D[2:m])
    } else if (j == (m-1))  {
      s <- off %% D[m]
    } else  {
      s <- (off %/% prod(D[(j+2):m])) %% D[j+1]
    }
    s
  }, numeric(1))
  X <- X+1
  X
}

# private - extract column-major linear index for subscripts
cidx4sub <- function(D, S)  {
  m <- length(D)
  S <- S-1
  off <- 0
  for (j in 0:(m-1))  {
    p <- ifelse(j == 0, 1, prod(D[1:j]))
    off <- off + S[j+1]*p
  }
  off+1
}

# private - extract row-major linear index for subscripts
ridx4sub <- function(D, S)  {
  m <- length(D)
  S <- S-1
  off <- 0
  for (j in 0:(m-1))  {
    p <- ifelse(j == m-1, 1, prod(D[(j+2):m]))
    off <- off + S[j+1]*p
  }
  off+1
}



# private - length of valid (Python) slice
slicelen <- function(slc)  {
  ss <- as.numeric(strsplit(slc, ':')[[1]])
  sdim <- (ss[2]-ss[1]) %/% ss[3]
  if ((ss[2]-ss[1]) %% ss[3] != 0)
    sdim <- sdim + 1
  sdim
}


# private - extract binary data from response
# reference: https://support.hdfgroup.org/HDF5/doc1.8/RM/PredefDTypes.html
extractBinary <- function(typ, nele, rsp)  {

  # standard defaults
  what <- 'integer'
  size <- NA_integer_
  signed <- TRUE
  endian <- .Platform$endian     # just a guess - the server determines the endianness

  df <- strcapture('H5T_([[:alnum:]]*)_([IFUBD])([[:digit:]]+)([LB]E)', typ$base, 
                   data.frame(cl=character(), wh=character(), sz=integer(), 
                              en=character(), stringsAsFactors=FALSE))
  if (nrow(df) != 1) 
    stop(paste0("binary transfer for type ", typ$base, " not implemented yet"))
  if (!(df[1,1] %in% c('STD', 'IEEE')))
    stop(paste0("binary transfer for type ", typ$base, " not implemented yet"))

  if (df[1,2] == 'I' && df[1,3] == '32')  {
    what <- 'integer'
    size <- 4
  } else if (df[1,2] == 'I' && df[1,3] == '64')  {
    what <- 'integer'
    size <- 8
  } else if (df[1,2] == 'F' && df[1,3] == '64')  {
    what <- 'double'
    size <- 8
  } else  {
    stop(paste0("binary transfer for type ", typ$base, " not implemented yet"))
  }
  endian <- ifelse(df[1,4] == 'LE', 'little', 'big')
  result <- readBin(rsp$content, what=what, n=nele, size=size, signed=signed, endian=endian)
  result

}

