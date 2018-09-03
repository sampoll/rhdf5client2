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


#' data retrieval 
#' @rdname extract-methods
#' @param dataset an object of type Dataset
#' @param indexlist a list of index vectors
#' @param transfermode either 'JSON' or 'binarytransfer'
#' @export

# QUESTION: set default transfermode to 'binary'?
#' special case: one-dimensional arrays
setMethod('[', c("Dataset", "numeric"), 
  function(x, i) {
    getDataList(x, list(i), transfermode='JSON')
  })

#' special case: two-dimensional arrays
setMethod('[', c("Dataset", "numeric", "numeric"), 
  function(x, i, j) {
    getDataList(x, list(i, j), transfermode='JSON')
  })


#' getData basic data-fetching functionality 
#' @name getData
#' @param dataset an object of type Dataset
#' @param indices array of valid character slices in R format or list of vectors
#' @export
setGeneric("getData", function(dataset, indices, transfermode) standardGeneric("getData"))
setMethod("getData", c("Dataset", "character", "character"),  
  function(dataset, indices, transfermode)  {
    getDataVec(dataset, indices, transfermode)
  })
setMethod("getData", c("Dataset", "character", "missing"),  
  function(dataset, indices)  {
    getDataVec(dataset, indices, 'JSON')
  })

setMethod("getData", c("Dataset", "list", "character"),  
function(dataset, indices, transfermode)  {
  getDataList(dataset, indices, transfermode)  
  })
setMethod("getData", c("Dataset", "list", "missing"),  
function(dataset, indices)  {
  getDataList(dataset, indices, 'JSON')  
  })


# private
getDataVec <- function(dataset, indices, transfermode = 'JSON')  {
    indices <- checkSlices(dataset@shape, indices)
    if (length(indices) == 0)
      stop("bad slices")
    if (!(transfermode %in% c('JSON', 'binary')))  {
      warning('unrecognized transfermode, using JSON')
      transfermode <- 'JSON'
    }

    sdims <- vapply(indices, slicelen, numeric(1))
    indices <- vapply(indices, function(s) { s }, character(1))
    sel <- paste0('[', paste(indices, collapse=','), ']')
    endpoint <- dataset@file@src@endpoint
    domain <- dataset@file@domain

    request <- paste0(endpoint, '/datasets/', dataset@uuid, 
      '/value?domain=', domain, '&select=', sel)
    response <- submitRequest(request, transfermode=transfermode)

    nn <- prod(sdims)
    A <- array(rep(0, nn))
    if (transfermode == 'JSON')  {
      result <- response$value
      # one-dimensional data is returned as a vector, 
      # multi-dimensional data is returned as a list.

      # unpack response into an R array 
      if (is.list(result))  {
        A[1:nn] <- vapply(1:nn, function(i) {
          idx <- csub4idx(sdims, i)
          result[[idx]]
        }, numeric(1))
      } else  {
        A[1:nn] <- result
      }
    } else if (transfermode == 'binary')  {
      result <- extractBinary(dataset@type, nn, response)
      A[1:nn] <- vapply(1:nn, function(i) {
        idx <- ridx4sub(sdims, csub4idx(sdims, i)) 
        result[idx]
      }, numeric(1))
    }
    AA <- array(A, dim = sdims)
}

# private
getDataList <- function(dataset, indices, transfermode = 'JSON')  {
    if (length(dataset@shape) != length(indices))
      stop("wrong length of indexlist")

    slicelist <- lapply(indices, slicify)
    slclen <- lapply(slicelist, length)

  # TODO: assemble block arrays with abind - general case
    if (any(slclen > 1))  {
      stop("assembly of block arrays not implemented yet")
    }
    AA <- getData(dataset, unlist(slicelist), transfermode)

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

# slicify - convert an arbitrary vector into slices
# This is an unsightly kludge, but it is designed to ensure
# that there is no more than one slice of width one.
# All other singletons should end up squashed into 
# pairs with each other. The idea is that the time 
# required to execute the loop is dwarfed by the time
# required to execute an extra remote fetch.

slicify <- function(v)  {
  ll <- vector("list", length = length(v))   
  vec <- rep(0, length(v))
  
  il <- 1
  if (length(v) <= 2)  {
    ll <- list(v)
  } else {
  
    vec[1] <- v[1]
    vec[2] <- v[2]
    nv <- 2
    i <- 3
    while (i <= length(v))  {
      if (v[i]-v[i-1] == v[i-1]-v[i-2])  {
        nv <- nv + 1
        vec[nv] <- v[i]
        i = i + 1
      } else  {
        ll[[il]] <- vec[1:nv]
        il <- il + 1
        if (i < length(v))  {
          vec[1] <- v[i]
          vec[2] <- v[i+1]
          nv <- 2
          i = i + 2
        } else {
          vec[1] <- v[i]
          nv <- 1
          i = i + 1
        }
     }
    }
    if (nv > 0)  {
      ll[[il]] <- vec[1:nv]
    }
    ll <- ll[-which(sapply(ll, is.null))]
  }
  
  slices <- vapply(ll, function(vec) { 
    start <- vec[1] 
    stop <- vec[length(vec)] 
    step <- ifelse(length(vec) == 1, 1, vec[2]-vec[1])
    sprintf("%d:%d:%d", start, stop, step)
  }, character(1))

}

