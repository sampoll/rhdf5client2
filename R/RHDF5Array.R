#' RHDF5ArraySeed for RHDF5Array backend to DelayedArray
#' @import DelayedArray
#' @exportClass RHDF5ArraySeed
setClass("RHDF5ArraySeed", contains=c("Array"), 
  slots = c(endpoint="character",    # URL
            svrtype="character",     # 'h5serv' or 'hsds'
            domain="character",      # extra-file (file system) path
            dsetname="character",    # complete intra-file path
            dataset="Dataset")       # Dataset object 
)

#' seed constructor 
#' @export RHDF5ArraySeed
RHDF5ArraySeed <- function(endpoint, svrtype, domain, dsetname)  {
  src <- rhdf5client2::Source(endpoint, svrtype)
  fle <- rhdf5client2::File(src, domain)
  dset <- rhdf5client2::Dataset(fle, dsetname)
  new("RHDF5ArraySeed", endpoint=endpoint, svrtype=svrtype, domain=domain, 
      dsetname=dsetname, dataset=dset)
}

#' HDF server content is assumed transposed relative to R matrix layout.
#' @export
setMethod("dimnames", "RHDF5ArraySeed", function(x)  {
  n <- length(x@dataset@shape)
  rt <- vector(mode="list", length=n)   # null list
})

#' @export
setMethod("dim", "RHDF5ArraySeed", function(x)  {
   as.integer(rev(x@dataset@shape))   # Could be this??? Don't assign?
})

#' @export
setMethod("extract_array", "RHDF5ArraySeed", function(x, index)  {
  index <- rev(index)

  # two special cases
  # (i) NULL index - signifies all elements in this dimension
  # (ii) zero-length index - signifies zero elements in this dimension
  # which means a null fetch 

  idxlist <- lapply(seq_along(index), 
    function(i)  {
      if (is.null(index[[i]]))  {
        n <- x@dataset@shape[i]
        if (n == 0)  {
          v <- numeric(0)
        } else  {
          v <- seq(1,n)
        }
      } else if (length(index[[i]]) == 0)  {
        v <- numeric(0)
      } else  {
        v <- unlist(slicelst(index[[i]]))
      }
      v
    }) 

  rdims <- lapply(idxlist, function(v) length(v))
  nullfetch <- any(rdims == 0)
  if (nullfetch) { 
    A <- array(numeric(0), dim=rdims)
  } else  {
    A <- rhdf5client2:::getDataList(x@dataset, idxlist)
    # unflatten vector if necessary: see note at 
    # end of Dataset::getDataVec on flattened result
    # for fetch of single-width dimensioned arrays.
    A <- array(A, dim=rdims)
  }
  R <- t(A)   # untranspose the transpose
  R
})

#' @exportClass RHDF5Array
setClass("RHDF5Array", contains="DelayedArray")

#' @exportClass RHDF5Matrix
setClass("RHDF5Matrix", contains=c("DelayedMatrix", "RHDF5Array"))

setMethod("matrixClass", "RHDF5Array", function(x) "RHDF5Matrix")

# Theory 1: there really is a problem with the RHDF5Matrix not having
# two dimensions. When this is not exported, an invalid RHDF5Matrix
# object is constructed but no error message is printed.

# Theory 2: there really is not a problem with the RHDF5Matrix not 
# having two dimensions. When this is not exported, an valid RHDF5Matrix
# but some bug in the DelayedArray infrastructure causes the 
# error message to be printed.

# Just doing: build(".", vignette=F) install(vignette=F) causes the 
# tests to run with no difficulty.

# Running either document() or build_vignettes() causes the tests 
# to crash.

# It is possibly a problem with as.integer() missing somewhere.

# Fact: calling dim on the DelayedMatrix does not call dim on 
# its seed, which it seems like it should.

#' @export
setAs("RHDF5Array", "RHDF5Matrix", function(from)  { 
    new("RHDF5Matrix", from)
  }
)

#' @export
setAs("RHDF5Matrix", "RHDF5Array", function(from) from)   # no-op

#' @importFrom DelayedArray new_DelayedArray
setMethod("DelayedArray", "RHDF5ArraySeed", 
  function(seed)   
    new_DelayedArray(seed, Class="RHDF5Array") 
)

#' @export
RHDF5Array <- function(endpoint, svrtype, domain, dsetname)  {
  DelayedArray(RHDF5ArraySeed(endpoint, svrtype, domain, dsetname))
}


