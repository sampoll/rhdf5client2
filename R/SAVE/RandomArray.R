#' RHDF5ArraySeed for RHDF5Array backend to DelayedArray
#' @import DelayedArray
#' @exportClass RHDF5ArraySeed
setClass("RHDF5ArraySeed", contains=c("Array"), 
  slots = c(filepath="character", dims="numeric")
)

#' seed constructor 
#' @export RHDF5ArraySeed
#' @importFrom tools file_path_as_absolute
RHDF5ArraySeed <- function(filepath)  {
  filepath <- file_path_as_absolute(filepath)
  con <- file(filepath, 'r')
  line <- readLines(con, n = 1)
  close(con)
  dd <- strsplit(line, '\\s+')
  dims <- as.integer(unlist(dd))
  # dims <- as.numeric(unlist(dd))      # NOT this

  new("RHDF5ArraySeed", filepath=filepath, dims=dims)
}

#' @export
setMethod("dim", "RHDF5ArraySeed", function(x)  {
  x@dims
})

#' @export
setMethod("extract_array", "RHDF5ArraySeed", function(x, index)  {
  browser()
  rdims <- vapply(index, length, numeric(1))

  if (any(rdims == 0))  {
    R <- array(numeric(0), dim=rdims)
  } else {
    which.null <- which(is.null(rdims))
    rdims[which.null] <- x@dims[which.null]
    R <- array(data=sample(prod(rdims)), dim=rdims)
  }
  R
})

#' @exportClass RHDF5Array
setClass("RHDF5Array", contains="DelayedArray")


