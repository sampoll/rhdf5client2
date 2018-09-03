# TODO: roxygen

#' RHDF5ArraySeed for RHDF5Array backend to DelayedArray
#' @import DelayedArray
setClass("RHDF5ArraySeed", contains=c("Array"), 
  slots = c(endpoint="character",    # URL
            svrtype="character",     # 'h5serv' or 'hsds'
            domain="character",      # extra-file (file system) path
            dsetname="character",    # complete intra-file path
            dataset="Dataset")       # Dataset object 
)

#' seed constructor 
#' @export
RHDF5ArraySeed <- function(endpoint, svrtype, domain, dsetname)  {
# requireNamespace(rhdf5client2)
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
  vector(mode="list", length=n)   # null list
})

#' @export
setMethod("dim", "RHDF5ArraySeed", function(x)  {
  dims <- as.integer(x@dataset@shape)
  rev(dims)
})

#' @export
setMethod("extract_array", "RHDF5ArraySeed", function(x, index)  {

  # requireNamespace(rhdf5client2)
  index <- rev(index)
  
  # two special cases
  # (i) NULL index - signifies all elements in this dimension
  # (ii) zero-length index - signifies zero elements in this dimension
  # which means a null fetch 

  idxlist <- lapply(index, 
    function(idx)  {
      slc <- ':'
      if (length(idx) == 0)  {
        slc <- '(null)'
      } else {
        slc <- slicify(idx)
        if (length(slc) != 1)
          stop("multiple slice indices not implemented yet")
        slc
      }
      slc
    }) 
  nullfetch <- any(idxlist == '(null)')

  rdims <- vapply(index, length, numeric(1)) 
  if (nullfetch) { 
    A <- array(numeric(0), dim=rdims)
  } else  {
    idxlist <- unlist(idxlist)
    A <- rhdf5client2::getData(x@dataset, idxlist)
  }
  t(A)

})

#' @exportClass RHDF5Array
setClass("RHDF5Array", contains="DelayedArray")

#' @export
# setClass("RHDF5Matrix", contains=c("DelayedArray", "RHDF5Array"))

#' @export
# setMethod("matrixClass", "RHDF5Array", function(x) "RHDF5Matrix")

#' @export
# setAs("RHDF5Array", "RHDF5Matrix", function(from)  {
#   new("RHDF5Matrix", from)
# })

#' @export
setMethod("DelayedArray", "RHDF5ArraySeed", 
  function(seed)  { new_DelayedArray(seed, Class="RHDF5Array") }
)

#' @export
RHDF5Array <- function(endpoint, svrtype, domain, dsetname)  {
  DelayedArray(RHDF5ArraySeed(endpoint, svrtype, domain, dsetname))
}













