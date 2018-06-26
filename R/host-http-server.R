#' A HTTP server for a Host
#'
#' Normally there is no need to create a new \code{HostHttpServer}, instead
#' use the \code{start} method of the \code{host} instance.
#'
#' Note for developers: the general approach taken in developing this class is
#' to mirror implementations in Node and Python to make it easier to port
#' code between the languages and ensure consisten implementations.
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
    #' }
    initialize = function(host, address="127.0.0.1", port=2000) {
      private$.host <- host
      private$.address <- address
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
            httpuv::startServer(private$.address, private$.port, list(call = self$handle)),
            error = identity
          )
          if (inherits(result, "error")) {
            if (result$message == "Failed to create server") {
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
        # Create request, getthing variables from request environment
        # See https://github.com/jeffreyhorner/Rook#the-environment for further details.
        request <- list(
          path = httpuv::decodeURIComponent(env$PATH_INFO),
          query = if (!is.null(env$QUERY_STRING)) httpuv::decodeURIComponent(env$QUERY_STRING) else "",
          method = env$REQUEST_METHOD,
          headers = list(
            Accept = env$HTTP_ACCEPT,
            Authorization = env$HTTP_AUTHORIZATION,
            Cookie = env$HTTP_COOKIE,
            Referer = env$HTTP_REFERER,
            Origin = env$HTTP_ORIGIN
          ),
          body = paste(env$rook.input$read_lines(), collapse = "")
        )

        # Create empty response
        response <- list(
          body = "",
          status = 200,
          headers = list()
        )

        # Check authorization
        authorized <- FALSE
        if (is.null(private$.host$key)) {
          authorized <- TRUE
        } else {
          auth_header <- request$headers$Authorization
          if (!is.null(auth_header)) {
            token <- str_match(auth_header, "^Bearer (.+)")[, 2]
            payload <- tryCatch(private$.host$authorize_token(token))
            if (inherits(payload, "error")) {
              return (self$error403(request, response, toString(error)))
            } else {
              authorized <- TRUE
            }
          }
        }

        # Add CORS headers used to control access by browsers. In particular, CORS
        # can prevent access by XHR requests made by Javascript in third party sites.
        # See https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS

        # Get the Origin header (sent in CORS and POST requests) and fall back to Referer header
        # if it is not present (either of these should be present in most browser requests)
        origin <- request$headers$Origin
        if (is.null(origin) & !is.null(request$headers$Referer)) {
          origin <- str_match(request$headers$Referer, "^https?://([\\w.]+)(:\\d+)?")[1, 1]
        }

        # Check that origin is in whitelist of file://, http://127.0.0.1, http://localhost, or http://*.stenci.la
        # The origin "file://" is sent when a connection is made from Electron (i.e Stencila Desktop)
        if (!is.null(origin)) {
          if (origin != "file://") {
            host <- str_match(origin, "^https?://([\\w.]+)(:\\d+)?")[1, 2]
            if (!str_detect(host, "(127\\.0\\.0\\.1)|(localhost)|(([^.]+\\.)?stenci.la)$")) origin <- NULL
          }
        }

        # If an origin has been found and is authorized set CORS headers
        # Without these headers browser XHR request get an error like:
        #     No "Access-Control-Allow-Origin" header is present on the requested resource.
        #     Origin "http://evil.hackers:4000" is therefore not allowed access.
        if (!is.null(origin)) {
          # "Simple" requests (GET and POST XHR requests)
          response$headers <- c(response$headers, list(
            "Access-Control-Allow-Origin" = origin,
            # Allow sending cookies and other credentials
            "Access-Control-Allow-Credentials" = "true"
          ))
          # Pre-flighted requests by OPTIONS method (made before PUT, DELETE etc XHR requests and in other circumstances)
          # get additional CORS headers
          if (request$method == "OPTIONS") {
            response$headers <- c(response$headers, list(
              # Allowable methods and headers
              "Access-Control-Allow-Methods" = "GET, POST, PUT, DELETE, OPTIONS",
              "Access-Control-Allow-Headers" = "Authorization, Content-Type",
              # "how long the response to the preflight request can be cached for without sending another preflight request"
              "Access-Control-Max-Age" = "86400" # 24 hours
            ))
          }
        }

        if (request$method == "OPTIONS") {
          # For preflighted CORS OPTIONS requests return an empty response with headers set
          # (https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS#Preflighted_requests)
          return(response)
        } else {
          endpoint <- self$route(request$method, request$path, authorized)
          method_name <- endpoint[1]
          method_args <- endpoint[2:length(endpoint)]
          method <- self[[method_name]]
          response <- do.call(method, c(list(request = request, response = response), method_args))
          return(response)
        }
      }, error = identity) # nolint (end of tryCatch block)

      if (inherits(response, "error")) self$error500(request, response)
      else response
    },

    #' @section route():
    #'
    #' Route a HTTP request
    route = function(verb, path = NULL, authorized = FALSE) {
      if (path == "/") return(c("static", "index.html"))
      if (str_sub(path, 1, 8) == "/static/") return(c("static", str_sub(path, 9)))

      version <- str_match(path, "^/(v\\d+)")[, 2]
      if (is.na(version)) {
        # Unversioned API endpoints
        if (path == "/manifest") return(c("run", "manifest"))

        if (!authorized) return(c("error401", path))

        if (str_sub(path, 1, 9) == "/environ/") {
          if (verb == "POST") return(c("run", "startup", str_sub(path, 10)))
          if (verb == "DELETE") return(c("run", "shutdown", str_sub(path, 10)))
        }

        match <- str_match(path, "^/(.+?)(!(.+))?$")
        if (!is.na(match[1, 1])) {
          id <- match[1, 2]
          method <- match[1, 4]
          if (verb == "POST" & !is.null(id)) return(c("run", "create", id))
          if (verb == "DELETE" & !is.null(id)) return(c("run", "destroy", id))
          if (verb == "PUT" & !is.null(id) & !is.null(method)) return(c("run", "call", id, method))
        }
      } else if (version == "v1") {
        # Versioned API endpoints
        parts <- str_split(path, "/")[[1]][-1]
        resource <- parts[2]

        if (verb == "GET" && resource %in% c("manifest", "environs", "services")) return(c("run", resource))

        if (!authorized) return(c("error401", path))

        if (resource == "hosts") {
          id <- parts[3]
          if (verb == "GET") return(c("run", "hosts"))
          if (verb == "POST" && !is.null(id)) return(c("run", "startup", id))
          if (verb == "DELETE" && !is.null(id)) return(c("run", "shutdown", id))
        }

        if (resource == "instances") {
          id <- parts[3]
          method <- parts[4]
          if (verb == "GET") return(c("run", "instances"))
          if (verb == "POST" && !is.null(id)) return(c("run", "create", id))
          if (verb == "DELETE" && !is.null(id)) return(c("run", "destroy", id))
          if (verb == "PUT" && !is.null(id) & !is.null(method)) return(c("run", "call", id, method))
        }
      }

      return(c("error400", path))
    },

    #' @section static():
    #'
    #' Handle a request for a static file
    static = function(request, response, path) {
      static_path <- normalizePath(system.file("static", package = "stencila"), winslash = "/")
      requested_path <- suppressWarnings(normalizePath(file.path(static_path, path), winslash = "/"))
      if (!str_detect(requested_path, paste0("^", static_path)) | str_detect(requested_path, "\\.\\./")) {
        # Don't allow any request outside of static folder
        self$error403(request, response, requested_path)
      } else if (!file.exists(requested_path)) {
        self$error404(request, response, requested_path)
      } else {
        file_connection <- file(requested_path, "r")
        lines <- suppressWarnings(readLines(file_connection))
        content <- paste(lines, collapse = "\n")
        close(file_connection)
        mimetype <- mime::guess_type(path)

        response$body <- content
        response$headers["Content-Type"] <- mimetype
        response
      }
    },

    #' @section run():
    #'
    #' Handle a request to call a host method
    run = function(request, response, method, ...) {
      args <- list(...)
      if (!is.null(request$body) && nchar(request$body) > 0) {
        args[[length(args) + 1]] <- from_json(request$body)
      }
      result <- do.call(private$.host[[method]], args)

      response$body <- to_json(result)
      response$headers["Content-Type"] <- "application/json"
      response
    },

    #' @section error():
    #'
    #' Generate an error response
    error = function(request, response, code, name, what = "") {
      response$status <- code
      response$body <- paste0(name, ": ", what)
      response$headers["Content-Type"] <- "text/plain"
      response
    },

    #' @section error400():
    #'
    #' Generate a 400 error
    error400 = function(request, response, what = "") {
      self$error(request, response, 400, "Bad request", what)
    },

    #' @section error401():
    #'
    #' Generate a 401 error
    error401 = function(request, response, what = "") {
      self$error(request, response, 401, "Unauthorized", what)
    },

    #' @section error403():
    #'
    #' Generate a 403 error
    error403 = function(request, response, what = "") {
      self$error(request, response, 403, "Forbidden", what)
    },

    #' @section error404():
    #'
    #' Generate a 404 error
    error404 = function(request, response, what = "") {
      self$error(request, response, 404, "Not found", what)
    },

    #' @section error500():
    #'
    #' Generate a 500 error
    error500 = function(request, response, error) {
      self$error(request, response, 500, "Internal error", toString(error))  # nocov
    }
  ),

  active = list(
    #' @section address:
    #'
    #' The address of the server
    address = function() {
      private$.address
    },

    #' @section port:
    #'
    #' The port of the server
    port = function() {
      private$.port
    },

    #' @section url:
    #'
    #' The URL of the server, or \code{NULL} if not yet started
    url = function() {
      if (is.null(private$.server)) NULL
      else paste0("http://", private$.address, ":", private$.port)
    }
  ),

  private = list(
    .host = NULL,
    .address = NULL,
    .port = NULL,
    .server = NULL
  )
)
