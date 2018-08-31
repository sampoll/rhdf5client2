setClass("File", representation(src="Source", domain="character"))

#' constructor for a File 
#' @name File
#' @param src an object of type Source
#' @param domain domain string
#' @examples
#' src.hsds <- Source('http://hsdshdflab.hdfgroup.org')
#' src.test <- Source('http://54.87.224.110:5000', 'h5serv')
#' src.chan <- Source('http://h5s.channingremotedata.org:5000', 'h5serv')
#' f <- File(src.test, '/fileone/subdirb/data/hdfgroup/org')
#' listDatasets(f)
#' f <- File(src.test, '/filetwo/subdirb/data/hdfgroup/org')
#' listDatasets(f)
#' f <- File(src.hsds, '/home/spollack/testzero.h5')
#' listDatasets(f)
#' @export
File <- function(src, domain)  {
  res <- rhdf5client2:::domainContents(src, domain)
  if (res[[1]]$filetype != 'self')
    stop("not a file")

  # h5serv requires dots (not slash) here and no leading dot
  if (src@type == 'h5serv')  {      
    domain <- gsub(.Platform$file.sep, '.', domain)
    if (substr(domain, 1, 1) == '.') 
      domain <- substr(domain, 2, nchar(domain))
  }
  obj <- new("File", src=src, domain=domain)
}

#' search inner file hierarchy
#' @name file an object of type File
#' @return a data frame with dataset inner-paths and UUIDs
#' @export
listDatasets <- function(file)  {
  if (!('File' %in% class(file)))
    stop("not a file")

  request <- paste0(file@src@endpoint, '?domain=', file@domain)
  response <- submitRequest(request)
  fileroot <- response$root

  # ye olde depth-first search
  eee <- new.env(parent=emptyenv())
  eee$results <- c()            # paths to datasets
  eee$uuids <- c()

  search <- function(uuid, path, ee)  {
    # ee$results <- c(ee$results, path)
    request <- paste0(file@src@endpoint, '/groups/', uuid, '/links?domain=', file@domain)
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
  data.frame(paths=eee$results, uuids=eee$uuids)

}

