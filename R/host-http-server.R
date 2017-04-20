#' A HTTP server for a Host
#'
#' Normally there is no need to create a new \code{HostHttpServer}, instead
#' use the \code{start} method of the \code{host} instance.
#'
#' @format \code{R6Class}.
HostHttpServer <- R6::R6Class("HostHttpServer",
  public = list(

    #' @section new():
    #'
    #' \describe{
    #'   \item{host}{The host to be served}
    #'   \item{port}{The port to listen on}
    #' }
    initialize = function(host, port=2000) {
      private$.host <- host
      private$.address <- '127.0.0.1'
      private$.port <- port
      private$.server <- NULL
    },

    #' @section start():
    #'
    #' Start the server
    start = function() {
      if (is.null(private$.server)) {
        while (private$.port < 65535) {
          result <- tryCatch(
            httpuv::startDaemonizedServer(private$.address, private$.port, list(call=self$handle)),
            error = identity
          )
          if (inherits(result, 'error')) {
            if (result$message == 'Failed to create server') {
              private$.port <- private$.port + 10
            } else {
              stop(result$message)
            }
          } else {
            private$.server <- result
            break
          }
        }
      }
    },

    #' @section stop():
    #'
    #' Stop the server
    stop = function() {
      if (!is.null(private$.server)) {
        httpuv::stopDaemonizedServer(private$.server)
        private$.server <- NULL
      }
    },

    #' @section handle():
    #'
    #' Handle a HTTP request
    handle = function(env) {
      # Get variables from request environment, possibilities include
      #
      #  [1] "HTTP_ACCEPT"                    "HTTP_ACCEPT_ENCODING"           "HTTP_ACCEPT_LANGUAGE"           "HTTP_CACHE_CONTROL"
      #  [5] "HTTP_CONNECTION"                "HTTP_COOKIE"                    "HTTP_HOST"                      "HTTP_UPGRADE_INSECURE_REQUESTS"
      #  [9] "HTTP_USER_AGENT"                "httpuv.version"                 "PATH_INFO"                      "QUERY_STRING"
      #  [13] "REMOTE_ADDR"                    "REMOTE_PORT"                    "REQUEST_METHOD"                 "rook.errors"
      #  [17] "rook.input"                     "rook.url_scheme"                "rook.version"                   "SCRIPT_NAME"
      #  [21] "SERVER_NAME"                    "SERVER_PORT"
      #
      # See https://github.com/jeffreyhorner/Rook/blob/a5e45f751/README.md#the-environment for
      request <- list(
        path = httpuv::decodeURIComponent(env$PATH_INFO),
        method = env$REQUEST_METHOD,
        headers = list(
          Accept = env$HTTP_ACCEPT
        ),
        body = paste(env$rook.input$read_lines(), collapse='')
      )
      response <- tryCatch({
          endpoint <- self$route(request$method, request$path)
          method <- endpoint[[1]]
          if (length(endpoint) == 1) method(request)
          else do.call(method, c(list(request=request), endpoint[2:length(endpoint)]))
        },
        error = identity
      )
      if (inherits(response, 'error')) {
        response <- self$error500(request, response)
      }
      # CORS access header added to all requests
      # See https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS
      response$headers <- c(response$headers, list(
        'Access-Control-Allow-Origin' = '*'
      ))
      response
    },

    #' @section route():
    #'
    #' Route a HTTP request
    route = function(verb, path) {
      if (verb == 'OPTIONS') return(list(self$options))

      if (path == '/') return(list(self$home))
      if (path == '/favicon.ico') return(list(self$static, 'favicon.ico'))
      if (str_sub(path, 1, 8) == '/static/') return(list(self$static, str_sub(path, 9)))

      match <- str_match(path, '^/(.+?)(!(.+))?$')
      if (!is.na(match[1, 1])) {
        id <- match[1, 2]
        method <- match[1, 4]
        if (verb == 'POST' & !is.null(id)) return(list(self$post, id))
        else if (verb == 'GET' & !is.null(id)) return(list(self$get, id))
        else if (verb == 'PUT' & !is.null(id) & !is.null(method)) return(list(self$put, id, method))
        else if (verb == 'DELETE' & !is.null(id)) return(list(self$delete, id))
      }

      return(NULL)
    },

    #' @section options():
    #'
    #' Handle OPTIONS request. Necessary for pre-flighted CORS requests.
    options = function(request) {
      list(body = '', status = 200, headers = list(
        'Access-Control-Allow-Methods' = 'GET, POST, PUT, DELETE, OPTIONS',
        'Access-Control-Allow-Headers' = 'Content-Type',
        'Access-Control-Max-Age' = '1728000'
      ))
    },

    #' @section home():
    #'
    #' Handle a request to `home`
    home = function(request) {
      if (!accepts_json(request)) {
        self$static(request, 'index.html')
      } else {
        list(
          body = to_json(private$.host$options()),
          status = 200,
          headers = list('Content-Type'='application/json')
        )
      }
    },

    #' @section static():
    #'
    #' Handle a request for a static file
    static = function(request, path) {
      static_path <- normalizePath(system.file('static', package = 'stencila'))
      requested_path <- suppressWarnings(normalizePath(file.path(static_path, path)))
      if (!str_detect(requested_path, paste0('^', static_path)) | str_detect(requested_path, '\\.\\./')) {
        # Don't allow any request outside of static folder
        self$error403()
      } else if (!file.exists(requested_path)) {
        self$error404()
      } else {
        file_connection <- file(requested_path, 'r')
        lines <- suppressWarnings(readLines(file_connection))
        content <- paste(lines, collapse='\n')
        close(file_connection)
        mimetype <- mime::guess_type(path)
        list(
          body = content,
          status = 200,
          headers = list('Content-Type'=mimetype)
        )
      }
    },

    #' @section post():
    #'
    #' Handle a POST request
    post = function(request, type) {
      list(
        body = to_json(private$.host$post(type, self$args(request)[['name']])),
        status = 200,
        headers = list('Content-Type'='application/json')
      )
    },

    #' @section post():
    #'
    #' Handle a GET request
    get = function(request, id) {
      list(
        body = to_json(private$.host$get(id)),
        status = 200,
        headers = list('Content-Type'='application/json')
      )
    },

    #' @section put():
    #'
    #' Handle a PUT request
    put = function(request, id, method) {
        list(
          body = to_json(private$.host$put(id, method, self$args(request))),
          status = 200,
          headers = list('Content-Type'='application/json')
        )
    },

    #' @section delete():
    #'
    #' Handle a DELETE request
    delete = function(request, id) {
      stop('Not yet implemeted')
    },

    args = function(request) {
      if (!is.null(request$body) && nchar(request$body) > 0) {
        # Vectors need to converted into a list for `do.call` below
        as.list(fromJSON(request$body, simplifyDataFrame=FALSE))
      } else {
        list()
      }
    },

    error403 = function(request, what='') {
      list(body = paste0('Unauthorized ', what), status = 403, headers = list('Content-Type' = 'text/html'))
    },

    error404 = function(request, what='') {
      list(body = paste0('Not found: ', what), status = 404, headers = list('Content-Type' = 'text/html'))
    },

    error500 = function(request, error) {
      list(body = paste0('Error: ', toString(error)), status = 500, headers = list('Content-Type' = 'text/html'))  # nocov
    }

  ),

  active = list(

    #' @section url:
    #'
    #' The URL of the server, or \code{NULL} if not yet started
    url = function() {
      if (is.null(private$.server)) NULL
      else paste0('http://', private$.address, ':', private$.port)
    }

  ),

  private = list(
    .host = NULL,
    .address = NULL,
    .port = NULL,
    .server = NULL
  )
)

# Does request accept JSON?
accepts_json <- function(request) {
  accept <- request$headers[['Accept']]
  if (is.null(accept)) FALSE
  else str_detect(accept, 'application/json')
}

# Convert a value to JSON
to_json <- function(value) {
  # jsonlite converts empty R lists to empty JSON arrays, override that
  if (is.list(value) & length(value)==0) '{}'
  else toString(toJSON(value, auto_unbox=TRUE, null='null'))
}
# Create a hook for conversion of R6 instances to JSON
methods::setClass('R6')
asJSON <- jsonlite:::asJSON
methods::setMethod('asJSON', 'R6', function(x, ...) {
  members <- list()
  for(name in ls(x, sorted=FALSE)) {
    if (!is.function(x[[name]])) members[[name]] <- x[[name]]
  }
  to_json(members)
})
