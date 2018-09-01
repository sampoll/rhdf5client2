setClass("File", representation(src="Source", domain="character", dsetdf="data.frame"))

#' constructor for a File 
#' @name File
#' @param src an object of type Source
#' @param domain domain string
#' @export
File <- function(src, domain)  {
  request <- paste0(src@endpoint, '?domain=', domain)
  try(response <- submitRequest(request))  # crashes if not a file domain
  dsetdf <- findDatasets(src, domain)
  obj <- new("File", src=src, domain=domain, dsetdf=dsetdf)
}

#' search inner file hierarchy
#' @name file an object of type File
#' @return a list of inner-paths 
#' @export
listDatasets <- function(file)  {
  file@dsetdf[['paths']]
}

#' private
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


