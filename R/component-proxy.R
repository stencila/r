
# R6 classes seem not to be ameinable to having getters and setting
# defined on the,. So, instead of using them we just use a plain
# old environment and define our own getters and setter using S3 methods
#
# We use syntax like `self[['.url']]` to bypass getters

ComponentProxy <- function(type, url) {
  self <- new.env(parent=emptyenv())

  self$.type <- type
  self$.url <- url

  # There is some sort of conflict between `httr` and `httpuv` packages
  # such that `httr` timeouts when making a request to the same port as
  # this host is listening on. Usually such a self-request will only be done
  # occasionally (e.g. during testing) . But to prevent these timeouts we bypass
  # making self-requests by checking to see if this component is actually local to this process
  com_parts <- parse_url(url)
  host_parts <- parse_url(host$url)
  if (com_parts$hostname == host_parts$hostname && com_parts$port == host_parts$port) {
    self$.local <- TRUE
    self$.address <- com_parts$path
  } else {
    self$.local <- FALSE
  }

  self[['dump']] <- function(format='data') {
    if (format == 'data') {
      list(
        type = self$.type,
        id = self$id,
        address = self$.address,
        url = self$.url
      )
    } else if (format == 'json') {
      toJSON(self$dump(), auto_unbox=TRUE)
    } else {
      stop(paste('Unhandled format:', format))
    }
  }

  self$.get <- function(name) {
    if (self$.local) {
      host$open(self$.address)[[name]]
    } else {
      response <- GET(
        paste0(self[['.url']], '!', name),
        accept_json(),
        timeout(10)
      )
      code <- response$status_code
      text <- content(response, 'text', encoding = 'UTF-8')
      if (code == 200 && nchar(text) > 0) {
        fromJSON(text)
      } else {
        stop(paste0('Error in proxy get\n  code: ', code, '\n  message: ',text))
      }
    }
  }

  self$.set <- function(name, value) {
    if (self$.local) {
      com <- host$open(self$.address)
      com[[name]] <- value
    } else {
        response <- PUT(
        paste0(self[['.url']], '!', name),
        accept_json(),
        content_type_json(),
        timeout(10),
        body = toJSON(value, auto_unbox = TRUE)
      )
      text <- content(response, 'text', encoding = 'UTF-8')
      if (response$status_code != 200) {
        stop(paste0('Error in proxy set:', text))
      }
    }
  }

  self$.call <- function(name, ...) {
    if (self$.local) {
      host$open(self$.address)[[name]](...)
    } else {
      response <- POST(
        paste0(self[['.url']], '!', name),
        accept_json(),
        content_type_json(),
        timeout(10),
        body = toJSON(list(...), auto_unbox = TRUE)
      )
      text <- content(response, 'text', encoding = 'UTF-8')
      if (response$status_code == 200) {
        if (nchar(text) > 0) {
          fromJSON(text)
        } else {
          invisible()
        }
      } else {
        stop(paste0('Error in proxy call:', text))
      }
    }
  }

  self[['show']] <- function(format='html') {
    self[['.call']]('show', format)
  }

  class(self) <- 'ComponentProxy'
  self
}

methods::setClass('ComponentProxy')

#' @export
'$.ComponentProxy' <- function(proxy, name) {
  if (exists(name, envir=proxy)) {
    get(name, envir=proxy)
  } else if (name == 'url' || name == 'type' || name == 'address') {
    get(paste0('.', name), envir=proxy)
  } else {
    get('.get', envir=proxy)(name)
  }
}

#' @export
'$<-.ComponentProxy' <- function(proxy, name, value) {
  get('.set', envir=proxy)(name, value)
  proxy
}

#' @export
'print.ComponentProxy' <- function(proxy) {
  cat(class(proxy)[1], '(', proxy[['.type']], ', ', proxy[['.url']], ')\n', sep='')
}

asJSON <- jsonlite:::asJSON
methods::setMethod("asJSON", "ComponentProxy", function(x, ...) {
  x$dump('json')
})
