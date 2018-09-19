#' RHDF5ArraySeed for RHDF5Array backend to DelayedArray
#'
#' @name RHDF5ArraySeed
#' @slot endpoint URL of remote server
#' @slot svrtype type of server, must be either 'hsds' or 'h5serv'
#' @slot domain HDF5 domain of H5 file on server
#' @slot dsetname complete internal path to dataset in H5 file
#' @slot dataset object of type Dataset for access to the H5 dataset
#' @aliases RHDF5ArraySeed-class
#' @import DelayedArray
#' @exportClass RHDF5ArraySeed
setClass("RHDF5ArraySeed", contains=c("Array"), 
  slots = c(endpoint="character",    # URL
            svrtype="character",     # 'h5serv' or 'hsds'
            domain="character",      # extra-file (file system) path
            dsetname="character",    # complete intra-file path
            dataset="Dataset")       # Dataset object 
)

#' Construct an object of type RHDF5ArraySeed 
#' 
#' @name RHDF5ArraySeed
#' 
#' @param endpoint URL of remote server
#' @param svrtype type of server, must be either 'hsds' or 'h5serv'
#' @param domain HDF5 domain of H5 file on server
#' @param dsetname complete internal path to dataset in H5 file
#' @return An initialized object of type RHDF5ArraySeed
#' @export RHDF5ArraySeed
RHDF5ArraySeed <- function(endpoint, svrtype, domain, dsetname)  {
  src <- rhdf5client2::Source(endpoint, svrtype)
  fle <- rhdf5client2::File(src, domain)
  dset <- rhdf5client2::Dataset(fle, dsetname)
  new("RHDF5ArraySeed", endpoint=endpoint, svrtype=svrtype, domain=domain, 
      dsetname=dsetname, dataset=dset)
}

#' Obtain names of dimensions for an object of type RHDF5ArraySeed
#' 
#' (required by DelayedArray seed contract, returns NULL list)
#' 
#' @name dimnames
#' @param x An object of type RHDF5ArraySeed
#' @return A NULL list of length equal to the array dimensionality
#' @aliases dimnames,RHDF5ArraySeed-method
#' @export
setMethod("dimnames", "RHDF5ArraySeed", function(x)  {
  n <- length(x@dataset@shape)
  rt <- vector(mode="list", length=n)   # null list
})

#' Obtain dimensions of an object of type RHDF5ArraySeed
#' 
#' (required by DelayedArray seed contract)
#' HDF server content is assumed transposed relative to R matrix layout.
#' This anticipates H5 datasets on the server with rows for 
#' experimental samples and columns for *-omic features. The 
#' Bioconductor SummarizedExperiment requires *-omic features in 
#' rows and samples in columns.
#' 
#' @name dim
#' @param x An object of type RHDF5ArraySeed
#' @return A numeric vector of the dimensions
#' @aliases dim,RHDF5ArraySeed-method
#' @export
setMethod("dim", "RHDF5ArraySeed", function(x)  {
   as.integer(rev(x@dataset@shape))   # Could be this??? Don't assign?
})

#' Access dataset backed by an RHDF5ArraySeed
#'
#' @name extract_array
#' @param x An object of type RHDF5ArraySeed
#' @param index A list of numeric vectors to be accessed, one vector 
#' for each dimension of the array object. A NULL vector indicates
#' the entire range of indices in that dimension. A zero-length
#' vector indicates no indices in the relevant dimension. (Accordingly,
#' any zero-length vector of indices will result in an empty array
#' being returned.)
#' 
#' @return An array containing the data elements corresponding to the 
#' indices requested
#' @aliases extract_array,RHDF5ArraySeed-method
#' 
#' @export
#'
# TODO: seed contract requires repeated and descending indices 
# (e.g., c(1,2,3,3,2,1)) be returned correctly. 
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

#' A DelayedArray backend for accessing a remote HDF5 server.
#' 
#' @name RHDF5Array 
#' @family  RHDF5Array
#' @aliases RHDF5Array-class
#' @exportClass RHDF5Array
setClass("RHDF5Array", contains="DelayedArray")

#' DelayedMatrix subclass for a two-dimensional RHDF5Array
#'
#' @name RHDF5Matrix
#' @family  RHDF5Array
#' @aliases RHDF5Matrix-class
#' @exportClass RHDF5Matrix
setClass("RHDF5Matrix", contains=c("DelayedMatrix", "RHDF5Array"))

setMethod("matrixClass", "RHDF5Array", function(x) "RHDF5Matrix")

#' @export
setAs("RHDF5Array", "RHDF5Matrix", function(from)  { 
    new("RHDF5Matrix", from)
  }
)

#' Coercion method from RHDF5Matrix to its superclass RHDF5Array
#'
#' @name as
#' @family  RHDF5Array
#' @export
setAs("RHDF5Matrix", "RHDF5Array", function(from) from)   # no-op

#' @importFrom DelayedArray new_DelayedArray
setMethod("DelayedArray", "RHDF5ArraySeed", 
  function(seed)   
    new_DelayedArray(seed, Class="RHDF5Array") 
)

#' Construct an object of type RHDF5Array directly from the data
#' members of its seed
#'
#' @param endpoint URL of remote server
#' @param svrtype type of server, must be either 'hsds' or 'h5serv'
#' @param domain HDF5 domain of H5 file on server
#' @param dsetname complete internal path to dataset in H5 file
#' @return An initialized object of type RHDF5Array
#' @export
RHDF5Array <- function(endpoint, svrtype, domain, dsetname)  {
  DelayedArray(RHDF5ArraySeed(endpoint, svrtype, domain, dsetname))
}
