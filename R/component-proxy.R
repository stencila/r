
# R6 classes seem not to be ameinable to having getters and setting
# defined on the,. So, instead of using them we just use a plain
# old environment and define our own getters and setter using S3 methods
#
# We use syntax like `self[['.url']]` to bypass getters

ComponentProxy <- function(type, url) {
  self <- new.env(parent=emptyenv())

  self$.type <- type
  self$.url <- url

  self$.get <- function(name) {
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

  self$.set <- function(name, value) {
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

  self$.call <- function(name, ...) {
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

  class(self) <- 'ComponentProxy'
  self
}

'$.ComponentProxy' <- function(proxy, name) {
  if (exists(name, envir=proxy)) {
    get(name, envir=proxy)
  } else if (name == 'url' || name == 'type') {
    get(paste0('.', name), envir=proxy)
  } else {
    get('.get', envir=proxy)(name)
  }
}

'$<-.ComponentProxy' <- function(proxy, name, value) {
  get('.set', envir=proxy)(name, value)
  proxy
}

'print.ComponentProxy' <- function(proxy) {
  cat(class(proxy)[1], '(', proxy[['.type']], ', ', proxy[['.url']], ')\n', sep='')
}
