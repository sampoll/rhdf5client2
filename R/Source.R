# All files are domains, but not all domains are files 
# The only non-file domains we care about are "root domains" 

# Given an endpoint and a root domain, what files are on
# the system? 

setClass("Source", representation(endpoint="character", type="character"))

#' constructor for a Source
#' @name Source
#' @param endpoint URL and port for server 
#' @param type Either 'h5serv' or 'hsds'
#' @examples
#' src.hsds <- Source('http://hsdshdflab.hdfgroup.org')
#' src.test <- Source('http://54.87.224.110:5000', 'h5serv')
#' src.chan <- Source('http://h5s.channingremotedata.org:5000', 'h5serv')
#' @export
Source <- function(endpoint, type='hsds')  {
  if (!(type %in% c('h5serv', 'hsds')))
    stop(paste("unknown server type ", type))
  obj <- new("Source", endpoint=endpoint, type=type)
  # member root id also?
}

#' files find files and subdirectories of a domain
#'
#' The user needs to give the domain to start in. The search
#' will be non-recursive. I.e., output for domain '/home/jreadey/' will 
#' not return the files in '/home/jreadey/HDFLabTutorial/'
setGeneric('files', function(object, rootdir) standardGeneric('files'))

#' @name files
#' @param object An object of type Source 
#' @param rootdir A slash-separated directory in the Source file system. 
#' @export
setMethod('files', c("Source", "character"), 
  function(object, rootdir)  {
    ll <- domainContents(object, rootdir)
    vapply(ll, function(l) l$filename, character(1))
  })

setMethod('files', c("Source", "missing"),  
  function(object) { 
    files(object, '/hdfgroup/org') 
  })


# private
domainContents <- function(object, rootdir = '/hdfgroup/org')  {
  if (!(is(object, "Source")))
    stop("getDomains called on a non-Source object")

  # result list
  rlistsz <- 1000
  rlist <- vector("list", rlistsz)
  nrlist <- 1

  append_to_results <- function(fn, ft)   {
    rlist[[nrlist]] <<- list(filename=fn, filetype=ft)
    nrlist <<- nrlist + 1
    if (nrlist >= rlistsz)  
      stop("list overflow")
  }

#' The h5serv API does not have a GET /domains method, so we need
#' to descend the tree to the requested domain node by UUID's.

  if (object@type == 'h5serv')  {
    append_if_h5_file <- function(ll, ff)  {
      if ('h5domain' %in% names(ll)) {
        ff <- c(ff, ll[['h5domain']])
      } 
      ff
    }

    # dissect the path. assume the last two elements are the root domain. 
    s <- .Platform$file.sep
    if (substr(rootdir, 1, 1) != s) rootdir <- paste0(s, rootdir)
    pth <- strsplit(rootdir, s)[[1]]
    pth <- pth[-1]      # empty string before leading '/'
    n <- length(pth)
    dstr <- paste0('.', pth[n-1], '.', pth[n])   # root domain
    if (length(pth) == 2)
      pth = c()
    else
      pth <- pth[1:(n-2)]

    request <- paste0(object@endpoint, "/")
    response <- submitRequest(request)
    nextid <- response[['root']]
    link <- list()

    while (length(pth) > 0)  {

      request <- paste0(object@endpoint, "/groups/", nextid, "/links")
      response <- submitRequest(request)
      links <- response[['links']]

      v <- vapply(links, function(lk) { 
        'title' %in% names(lk) && lk[['title']] == pth[length(pth)] }, 
        logical(1))
      if (!any(v))  
        stop(paste0("domain ",rootdir, " not found"))

      link <- links[[which(v)]]
      nextid <- link[['id']]
      dstr <- paste0(pth[length(pth)], '.', dstr)
      pth <- pth[-length(pth)]

    }

    if (length(link) == 0 || 
        link[['class']] == 'H5L_TYPE_HARD')  {  # domain is a directory
      request <- paste0(object@endpoint, "/groups/", nextid, "/links")
      response <- submitRequest(request)
      links <- response[['links']]
      for (lk in links)  {
        if (lk[['class']] == 'H5L_TYPE_HARD')  {             # a subdirectory
          append_to_results(paste0(lk[['title']], rootdir), 'directory')
        } else if (lk[['class']] == 'H5L_TYPE_EXTERNAL')  {  # a file
          append_to_results(lk[['h5domain']], 'file')
        }
      }

    } else  {  # 'self' signals that rootdir *is* a file 
      append_to_results(rootdir, 'self')
    }
  }
  else  { # 'hsds'

    # no postpended slash gets the type of the object
    request <- paste0(object@endpoint, "/domains?domain=", rootdir)
    response <- submitRequest(request)

    if ('domains' %in% names(response) && length(response[['domains']]) > 0)  {

      # with postpended slash gets directory contents
      if ('root' %in% names(response[['domains']][[1]]) && 
          response[['domains']][[1]][['root']] != 'None')  {
        fn <- rootdir
        ft <- 'self'
        append_to_results(fn, ft)
      } else  {
        request <- paste0(object@endpoint, "/domains?domain=", rootdir, "/")
        response <- submitRequest(request)
        domains <- response[['domains']]
        for (domain in domains)  {
          if ('class' %in% names(domain) && 'name' %in% names(domain) && 
             (domain[['class']] == 'domain' || domain[['class']] == 'folder'))  {
            fn <- domain[['name']]
            ft <- 'directory'
            if (domain[['class']] == 'domain') 
              ft <- 'file'
            append_to_results(fn, ft)
          }
        }
      }
    }
  }
  rlist[-which(sapply(rlist, is.null))]

}

# private
submitRequest <- function(req)  {
  rsp <- httr::GET(req)
  if (rsp$status_code != 200)
    stop(paste0("bad response, status code ", rsp$status_code))
  jsn <- readBin(rsp$content, what="character")
  rjson::fromJSON(jsn)
}

# Dataset object: everything necessary to acquire data from the dataset
# h5serv File 

# append the file's "title" to the link request: 
# request <- paste0(<endpoint>, "/groups/", <grpid>, "/links/", <title>)
# this is a valid request, but the response does not look very useful

# target <- paste0(<endpoint>, "/?host=", <h5domain>)    OR 
# target <- link[['target']]
# submitRequest(target)
# uuid <- response$root

# from target, we can query the database to see what is in the File:
# database <- paste0(<endpoint>, "/datasets?host=", <h5domain>)    
# groupbase <- paste0(<endpoint>, "/groups?host=", <h5domain>)    
# typebase <- paste0(<endpoint>, "/datatype?host=", <h5domain>)    




