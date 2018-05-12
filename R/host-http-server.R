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
    #'   \item{address}{The port to listen on. Default \code{'127.0.0.1'}}
    #'   \item{port}{The port to listen on. Default \code{2000}}
    #'   \item{authorization}{Authorization for all requests. Default \code{TRUE}}
    #' }
    initialize = function(host, address='127.0.0.1', port=2000, authorization=TRUE) {
      private$.host <- host
      private$.address <- address
      private$.port <- port

      auth <- Sys.getenv('STENCILA_AUTHORIZATION')
      if (auth == 'true') authorization = TRUE
      else if (auth == 'false') authorization = FALSE
      private$.authorization <- authorization

      private$.server <- NULL
      private$.tickets <- NULL
      private$.tokens <- NULL
    },

    #' @section start():
    #'
    #' Start the server
    start = function() {
      if (is.null(private$.server)) {
        while (private$.port < 65535) {
          result <- tryCatch(
            httpuv::startServer(private$.address, private$.port, list(call=self$handle)),
            error = identity
          )
          if (inherits(result, 'error')) {
            if (result$message == 'Failed to create server') {
              private$.port <- private$.port + 10
            } else {
              stop(result$message) # nocov
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
        httpuv::stopServer(private$.server)
        private$.server <- NULL
      }
    },

    #' @section handle():
    #'
    #' Handle a HTTP request
    handle = function(env) {
      response <- tryCatch({
        # Create request, getthing variables from request environment, possibilities include
        #
        #  [1] "HTTP_ACCEPT"                    "HTTP_ACCEPT_ENCODING"           "HTTP_ACCEPT_LANGUAGE"           "HTTP_CACHE_CONTROL"
        #  [5] "HTTP_CONNECTION"                "HTTP_COOKIE"                    "HTTP_HOST"                      "HTTP_UPGRADE_INSECURE_REQUESTS"
        #  [9] "HTTP_USER_AGENT"                "httpuv.version"                 "PATH_INFO"                      "QUERY_STRING"
        #  [13] "REMOTE_ADDR"                    "REMOTE_PORT"                    "REQUEST_METHOD"                 "rook.errors"
        #  [17] "rook.input"                     "rook.url_scheme"                "rook.version"                   "SCRIPT_NAME"
        #  [21] "SERVER_NAME"                    "SERVER_PORT"
        #
        # See https://github.com/jeffreyhorner/Rook#the-environment for further details.
        request <- list(
          path = httpuv::decodeURIComponent(env$PATH_INFO),
          query = if (!is.null(env$QUERY_STRING)) httpuv::decodeURIComponent(env$QUERY_STRING) else '',
          method = env$REQUEST_METHOD,
          headers = list(
            Accept = env$HTTP_ACCEPT,
            Cookie = env$HTTP_COOKIE
          ),
          body = paste(env$rook.input$read_lines(), collapse='')
        )

        # Check authorization. Note that browsers do not send credentials (e.g. cookies)
        # in OPTIONS requests
        cookie_header <- NULL
        if (private$.authorization & request$method != 'OPTIONS') {
          # Check for ticket
          ticket <- urltools::param_get(paste0(request$path, request$query), 'ticket')$ticket
          if(!is.na(ticket)) {
            # Check ticket is valid
            if (!self$ticket_check(ticket)) return(self$error_403(request, paste0(': invalid ticket : ', ticket)))
            else {
              # Set token cookie
              cookie_header <- list('Set-Cookie' = paste0('token=', self$token_create(), '; Path=/'))
            }
          } else {
            # Check for token
            cookie <- request$headers$Cookie
            token <- if (is.null(cookie)) NA else str_match(cookie, 'token=([a-zA-Z0-9]+)')[1,2]
            if (is.na(token) | !self$token_check(token)) return(self$error_403(request, paste0(': invalid token : ', token)))
          }
        }

        # Create response
        endpoint <- self$route(request$method, request$path)
        method <- endpoint[[1]]
        if (length(endpoint) == 1) response <- method(request)
        else response <- do.call(method, c(list(request=request), endpoint[2:length(endpoint)]))

        # Add CORS headers used to control access by browsers. In particular, CORS
        # can prevent access by XHR requests made by Javascript in third party sites.
        # See https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS

        # Get the Origin header (sent in CORS and POST requests) and fall back to Referer header
        # if it is not present (either of these should be present in most browser requests)
        origin <- env$HTTP_ORIGIN
        if (is.null(origin) & !is.null(env$HTTP_REFERER)) {
          origin <- str_match(env$HTTP_REFERER, '^https?://([\\w.]+)(:\\d+)?')[1,1]
        }

        # Check that origin is in whitelist of file://, http://127.0.0.1, http://localhost, or http://*.stenci.la
        # The origin 'file://' is sent when a connection is made from Electron (i.e Stencila Desktop)
        if (!is.null(origin)) {
          if (origin != 'file://') {
            host <- str_match(origin, '^https?://([\\w.]+)(:\\d+)?')[1,2]
            if (!str_detect(host, "(127\\.0\\.0\\.1)|(localhost)|(([^.]+\\.)?stenci.la)$")) origin <- NULL
          }
        }

        # If an origin has been found and is authorized set CORS headers
        # Without these headers browser XHR request get an error like:
        #     No 'Access-Control-Allow-Origin' header is present on the requested resource.
        #     Origin 'http://evil.hackers:4000' is therefore not allowed access.
        if (!is.null(origin)) {
          # 'Simple' requests (GET and POST XHR requests)
          cors_headers <- list(
            'Access-Control-Allow-Origin' = origin,
            # Allow sending cookies and other credentials
            'Access-Control-Allow-Credentials' = 'true'
          )
          # Pre-flighted requests by OPTIONS method (made before PUT, DELETE etc XHR requests and in other circumstances)
          # get additional CORS headers
          if (request$method == 'OPTIONS') {
            cors_headers <- c(cors_headers, list(
              # Allowable methods and headers
              'Access-Control-Allow-Methods' = 'GET, POST, PUT, DELETE, OPTIONS',
              'Access-Control-Allow-Headers' = 'Content-Type',
              # "how long the response to the preflight request can be cached for without sending another preflight request"
              'Access-Control-Max-Age' = '86400' # 24 hours
            ))
          }
          response$headers <- c(response$headers, cors_headers)
        }

        # Set token cookie if necessary
        if (!is.null(cookie_header)) response$headers <- c(response$headers, cookie_header)

        response

      }, error=identity)
      if (inherits(response, 'error')) self$error_500(request, response)
      else response
    },

    #' @section route():
    #'
    #' Route a HTTP request
    route = function(verb, path) {
      if (verb == 'OPTIONS') return(list(self$options))

      if (path == '/') return(list(self$home))
      if (path == '/manifest') return(list(self$manifest))
      if (path == '/favicon.ico') return(list(self$static, 'favicon.ico'))
      if (str_sub(path, 1, 8) == '/static/') return(list(self$static, str_sub(path, 9)))

      if (str_sub(path, 1, 9) == '/environ/') {
        if (verb == 'POST') return(list(self$startup, str_sub(path, 10)))
      }

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
    #' Handle a OPTIONS request
    options = function(request) {
      # It seems necessary to have at least one header set
      list(body = '', status = 200, headers = list('Content-Type'='text/plain'))
    },

    #' @section home():
    #'
    #' Handle a request to `home`
    home = function(request) {
      if (!self$accepts_json(request)) {
        self$static(request, 'index.html')
      } else {
        list(
          body = to_json(private$.host$manifest()),
          status = 200,
          headers = list('Content-Type'='application/json')
        )
      }
    },

    #' @section static():
    #'
    #' Handle a request for a static file
    static = function(request, path) {
      static_path <- normalizePath(system.file('static', package = 'stencila'), winslash='/')
      requested_path <- suppressWarnings(normalizePath(file.path(static_path, path), winslash='/'))
      if (!str_detect(requested_path, paste0('^', static_path)) | str_detect(requested_path, '\\.\\./')) {
        # Don't allow any request outside of static folder
        self$error_403()
      } else if (!file.exists(requested_path)) {
        self$error_404()
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

    #' @section manifest():
    #'
    #' Handle a request for the host's manifest
    manifest = function(request) {
      list(
        body = to_json(private$.host$manifest()),
        status = 200,
        headers = list('Content-Type'='application/json')
      )
    },

    #' @section startup():
    #'
    #' Handle a request to start and environment
    startup = function(request, type) {
      list(
        body = '',
        status = 200,
        headers = list('Content-Type'='application/json')
      )
    },

    #' @section post():
    #'
    #' Handle a POST request
    post = function(request, type) {
      list(
        body = to_json(private$.host$post(type, self$args(request))),
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

    #' @section args():
    #'
    #' Convert the body into a named list of arguments
    args = function(request) {
      if (!is.null(request$body) && nchar(request$body) > 0) {
        from_json(request$body)
      } else {
        list()
      }
    },

    #' @section accepts_json():
    #'
    #' Does a request accept JSON?
    accepts_json = function(request) {
      accept <- request$headers[['Accept']]
      if (is.null(accept)) FALSE
      else str_detect(accept, 'application/json')
    },

    #' @section ticket_create():
    #'
    #' Create a ticket (a single-use access token)
    ticket_create = function () {
      ticket <- paste(sample(c(LETTERS, letters, 0:9), 12, replace=TRUE), collapse='')
      private$.tickets <- c(private$.tickets, ticket)
      ticket
    },

    #' @section ticket_check():
    #'
    #' Check that a ticket is valid. If it is, then it is removed
    #' from the list of tickets and \code{TRUE} is returned. Otherwise,
    #' returns \code{FALSE}
    ticket_check = function (ticket) {
      if (ticket %in% private$.tickets) {
        private$.tickets <- setdiff(ticket, private$.tickets)
        TRUE
      } else {
        FALSE
      }
    },

    #' @section ticketed_url():
    #
    #' Create a URL with a ticket query parameter so users
    #' can connect to this server
    #'
    #' @return {string} A ticket
    ticketed_url = function () {
      url <- self$url
      if (private$.authorization) url <- paste0(url, '/?ticket=', self$ticket_create())
      url
    },

    #' @section token_create():
    #'
    #' Create a token (a multiple-use access token)
    token_create = function () {
      token <- paste(sample(c(LETTERS, letters, 0:9), 126, replace=TRUE), collapse='')
      private$.tokens <- c(private$.tokens, token)
      token
    },

    #' @section token_check():
    #'
    #' Check that a token is valid.
    token_check = function (token) {
      token %in% private$.tokens
    },

    #' @section error_403():
    #'
    #' Generate a 403 error
    error_403 = function(request, what='') {
      list(body = paste0('Unauthorized ', what), status = 403, headers = list('Content-Type' = 'text/html'))
    },

    #' @section error_404():
    #'
    #' Generate a 404 error
    error_404 = function(request, what='') {
      list(body = paste0('Not found ', what), status = 404, headers = list('Content-Type' = 'text/html'))
    },

    #' @section error_500():
    #'
    #' Generate a 500 error
    error_500 = function(request, error) {
      list(body = paste0('Error ', toString(error)), status = 500, headers = list('Content-Type' = 'text/html'))  # nocov
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
    .authorization = NULL,
    .server = NULL,
    .tickets = NULL,
    .tokens = NULL
  )
)

