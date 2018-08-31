# All files are domains, but not all domains are files 
# The only non-file domains we care about are "root domains" 

# Given an endpoint and a root domain, what files are on
# the system? 

setClass("Source", representation(endpoint="character", type="character"))

#' constructor for a Source
#' @name Source
#' @param endpoint URL and port for server 
#' @param type Either 'h5serv' or 'hsds'
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
  function(object, rootdir = '/hdfgroup/data')  {
  if (!(is(object, "Source")))
    stop("getDomains called on a non-Source object")

  retvec <- c()

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
          retvec <- c(retvec, paste0(lk[['title']], rootdir))
        } else if (lk[['class']] == 'H5L_TYPE_EXTERNAL')  {  # a file
          retvec <- c(retvec, lk[['h5domain']])
        }
      }

    } else  {
      retvec <- c(rootdir)    # domain is a File
    }
  }
  else  { # 'hsds'
    append_if_h5_file <- function(ll, ff)  {
      if ('class' %in% names(ll) && 'name' %in% names(ll) &&  
         ( ll[['class']] == 'domain' || ll[['class']] == 'folder' ) )
            ff <- c(ff, ll[['name']])   # a File
      ff
    }

    request <- paste0(object@endpoint, "/domains?domain=", rootdir, "/")
    response <- submitRequest(request)
    domains <- response[['domains']]
    for (domain in domains)  {
      retvec <- append_if_h5_file(domain, retvec)
    }

  }
  retvec

})


setMethod('files', c("Source", "missing"),  
  function(object) { files(object, '/hdfgroup/data') })


# private
submitRequest <- function(req)  {
  rsp <- httr::GET(req)
  if (rsp$status_code != 200)
    stop(paste0("bad response, status code ", rsp$status_code))
  jsn <- readBin(rsp$content, what="character")
  rjson::fromJSON(jsn)
}

# hide ugly stuff
lwhich <- function(ll, fl)  {
  which(vapply(ll, fl, logical(1)))
}


# Question: from the domain (file), how do we obtain the group/dataset 
# hierarchy inside the file? 
# Plan: look at how John does it in function visit.





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




