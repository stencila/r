#' A HTTP server for a `Host`
#'
#' Normally there is no need to create a new \code{HostHttpServer}, instead
#' use the \code{start} method of the \code{host} nstance.
#'
#' @name HttpServer
HostHttpServer <- R6Class("HttpServer",
  public = list(

    #' @section \code{initialize} method:
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

    #' @section \code{start} method:
    #'
    #' Start the server
    start = function() {
      if (is.null(private$.server)) {
        while (private$.port < 65535) {
          result <- tryCatch(
            startDaemonizedServer(private$.address, private$.port, list(call=self$handle)),
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

    #' @section \code{stop} method:
    #'
    #' Stop the server
    stop = function() {
      if (!is.null(private$.server)) {
        stopDaemonizedServer(private$.server)
        private$.server <- NULL
      }
    },

    #' @section \code{handle} method:
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
        path = decodeURIComponent(env$PATH_INFO),
        method = env$REQUEST_METHOD,
        body = paste(env$rook.input$read_lines(), collapse=''),
        accept = env$HTTP_ACCEPT
      )
      response <- tryCatch({
          endpoint <- self$route(request$method, request$path)
          method <- endpoint[[1]]
          args <- endpoint[2:length(endpoint)]
          args <- c(list(request=request), args)
          do.call(method, args)
        },
        error = identity
      )
      if (inherits(response, 'error')) {
        self$error500(request, response)
      } else {
        # CORS headers added to all requests to allow direct access by browsers
        # See https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS
        response$headers <- c(response$headers, list(
          'Access-Control-Allow-Origin' = '*',
          'Access-Control-Allow-Methods' = 'GET,POST,PUT,DELETE,OPTIONS',
          'Access-Control-Allow-Headers' = 'Content-Type',
          'Access-Control-Max-Age' = '1728000'
        ))
        response
      }
    },

    #' @section \code{route} method:
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

    #' @section \code{options} method:
    #'
    #' Handle OPTIONS request
    #' Necessary for preflighted CORS requests (https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS#Preflighted_requests)
    options = function(request) {
      list(body = '', status = 200, headers = list())
    },

    #' @section \code{home} method:
    #'
    #' Handle a request to `home`
    home = function(request) {
      if (!accepts_json(request)) {
        self$static(request, 'index.html')
      } else {
        list(
          body = jsonify(private$.host$options()),
          status = 200,
          headers = list('Content-Type'='application/json')
        )
      }
    },

    #' @section \code{home} method:
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

    post = function(request, type) {
      list(
        body = private$.host$post(type),
        status = 200,
        headers = list('Content-Type'='application/json')
      )
    },

    get = function(request, id) {
      list(
        body = jsonify(private$.host$get(id)),
        status = 200,
        headers = list('Content-Type'='application/json')
      )
    },

    put = function(request, address, name) {
      component = private$.host$open(address)
      if (!is.null(component)) {
        if (!is.null(request$body) && nchar(request$body) > 0) {
          args <- fromJSON(request$body, simplifyDataFrame=FALSE)
          # Vectors need to converted into a list for `do.call` below
          args <- as.list(args)
        } else {
          args <- list()
        }
        method <- component[[name]]
        if (!is.null(method)) {
          result <- do.call(method, args)
          if (inherits(result, 'Component')) {
            content <- result$dump('json')
          } else {
            content <- jsonify(result)
          }
          list(body = content, status = 200, headers = list('Content-Type' = 'application/json'))
        } else {
          self$error404(request, paste0(address, '$', name))
        }
      } else {
        self$error404(request, address)
      }
    },

    delete = function(){},

    error403 = function(request, what='') {
      list(body = paste0('Unauthorized ', what), status = 403, headers = list('Content-Type' = 'text/html'))
    },

    error404 = function(request, what='') {
      list(body = paste0('Not found: ', what), status = 404, headers = list('Content-Type' = 'text/html'))
    },

    error500 = function(request, error) {
      list(body = paste0('Error: ', toString(error)), status = 500, headers = list('Content-Type' = 'text/html'))
    }

  ),

  active = list(

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

accepts_json <- function(request) {
  accept <- request$headers[['Accept']]
  if (is.null(accept)) FALSE
  else str_detect(accept, 'application/json')
}

jsonify <- function(value) {
  toString(toJSON(value, auto_unbox=TRUE, null='null'))
}


