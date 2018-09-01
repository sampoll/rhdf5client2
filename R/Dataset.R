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
getData <- function(dataset, slices)  {
  slices <- checkSlices(dataset@shape, slices)
  if (length(slices) == 0)
    stop("bad slices")

  # convert to vector
  slices <- vapply(slices, function(s) { s }, character(1))

  sel <- paste0('[', paste(slices, collapse=','), ']')
  endpoint <- dataset@file@src@endpoint
  domain <- dataset@file@domain

  request <- paste0(endpoint, '/datasets/', dataset@uuid, 
    '/value?domain=', domain, '&select=', sel)

  response <- submitRequest(request)
  response$value

  # TODO: do.call(rbind, response$value) works for a 2D matrix
  # Use abind::abind to do the recursive multi-dimensional case.

  # Something like this:

  # multibind <- function(L, nl)  {
  #   if (nl < ndim)  {
  #     L <- lapply(L, function(LL) multibind(LL, along=nl-1))
  #   } 
  #   do.call(abind, L, along=nl)
  # }    

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
