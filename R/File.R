#' An S4 class to represent an HDF5 file accessible from a server.
#'
#' @slot Source an object of type Source
#' @slot domain the file's domain on the server; more or less, an alias for its 
#' location in the external server file system
#' @slot dsetdf a data.frame that caches often-used information about the file
setClass("File", representation(src="Source", domain="character", dsetdf="data.frame"))

#' Construct an object of type File
#'
#' A File is a representation of an HDF5 file the contents of which are accessible 
#' exposed by a HDF5 server. 
#'
#' @name File
#' @param src an object of type Source, the server which exposes the file
#' @param domain the domain string; the file's location on the server's
#' file system.
#' @examples
#' src <- Source('http://hsdshdflab.hdfgroup.org')
#' f10x <- File(src, '/shared/bioconductor/tenx_full.h5')
#' @export 
File <- function(src, domain)  {
  request <- paste0(src@endpoint, '?domain=', domain)
  try(response <- submitRequest(request))  # crashes if not a file domain
  dsetdf <- findDatasets(src, domain)
  obj <- new("File", src=src, domain=domain, dsetdf=dsetdf)
}

#' Search inner file hierarchy for datasets
#' 
#' The datasets in an HDF5 file are organized internally by groups.
#' This routine traverses the internal group hiearchy, locates
#' all datasets and prints a list of them. Note that if the 
#' file's group hiearchy is complex, this could be time-consuming.
#'
#' @param file an object of type File to be searched
#' 
#' @return a list of inner-paths 
#' 
#' @examples
#' src <- Source('http://hsdshdflab.hdfgroup.org')
#' f <- File(src, '/home/spollack/testzero.h5')
#' listDatasets(f)
#' @export
listDatasets <- function(file)  {
  file@dsetdf[['paths']]
}

#  private - traverse internal file hiearchy, find datasets, and
#  cache often-accessed information in a data.frame for the File object.
findDatasets <- function(src, domain)  {

  request <- paste0(src@endpoint, '?domain=', domain)
  response <- submitRequest(request)
  fileroot <- response$root

  # ye olde depth-first search
  eee <- new.env(parent=emptyenv())
  eee$results <- c()            # paths to datasets
  eee$uuids <- c()

  search <- function(uuid, path, ee)  {
    # ee$results <- c(ee$results, path)
    request <- paste0(src@endpoint, '/groups/', uuid, '/links?domain=', domain)
    response <- submitRequest(request)
    for (link in response[['links']])  {
      if ('collection' %in% names(link) && link[['collection']] == 'groups')  {
        nxtuuid <- link[['id']]
        nxtpath <- paste0(path, '/', link[['title']])
        search(nxtuuid, nxtpath, ee)
      } else if ('collection' %in% names(link) && link[['collection']] == 'datasets')  {
        nxtuuid <- link[['id']]
        nxtpath <- paste0(path, '/', link[['title']])
        ee$results <- c(ee$results, nxtpath)
        ee$uuids <- c(ee$uuids, nxtuuid)
      }
    }
  }

  search(fileroot, '', eee)
  data.frame(paths=eee$results, uuids=eee$uuids, stringsAsFactors = FALSE)

}
setMethod("show", "File", function(object) {
 cat(paste("rhdf5client2 File instance from source", object@src@endpoint, "\n"))
 cat(paste("  domain: ", object@domain, "\n"))
 cat("  use listDatasets(...) and Dataset(..., [dataset name]) for more content.\n")
})

#setMethod("show", "Dataset", function(object) {
# cat(paste("rhdf5client2 Dataset instance, with shape "))
# dput(object@shape)
# cat("  use getData(...) or square brackets to retrieve content.\n")
#})
