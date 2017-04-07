#' The host singleton instance
#'
#' @rdname Host
#' @export
host <- NULL

# List of types that hosts supports
#' @include r-context.R
TYPES <- list(
  RContext = RContext
)

#' A `Host` allows you to create, get, run methods of, and delete instances of various types.
#' The types can be thought of a "services" provided by the host e.g. `NoteContext`, `FileSystemStorer`
#' 
#' The API of a host is similar to that of a HTTP server. It's methods names
#' (e.g. `post`, `get`) are similar to HTTP methods (e.g. `POST`, `GET`) but
#' the sematics sometimes differ (e.g. a host's `put()` method is used to call an 
#' instance method)
#' 
#' A `Host` is not limited to beng served by HTTP and it's methods are exposed by both `HostHttpServer`
#' and `HostWebsocketServer`. Those other classes are responsible for tasks associated with
#' their communication protocol (e.g. serialising and deserialising objects).
#' 
#' This is a singleton class. There should only ever be one `Host`
#' in memory in each process (although, for purposes of testing, this is not enforced)
#'
#' @rdname Host
Host <- R6Class("Host",
  public = list(

    initialize = function () {
      private$.servers <- list()
      private$.instances <- list()
    },

    #' @section \code{options} method:
    #'
    #' Get a manifest for this host
    #'
    #' The manifest describes the host and it's capabilities. It is used
    #' by peer hosts to determine which "types" this host provides and
    #' which "instances" have already been instantiated.
    options = function () {
      list(
        stencila = list(
          package = 'r',
          version = version
        ),
        urls = sapply(private$.servers, function (server) server$url),
        types = names(TYPES),
        instances = names(private$.instances)
      )
    },

    #' @section \code{post} method:
    #'
    #' Create a new instance of a type
    #' 
    #' \describe{
    #'   \item{type}{Type of instance}
    #'   \item{options}{Options to be passed to type constructor}
    #' }
    #' 
    #' Returns the ID string of the newly created instance
    post = function (type, options = list()) {
      Class <- TYPES[[type]]
      if (!is.null(Class)) {
        instance <- Class$new()
        id <- paste(sample(c(letters,0:9), 10),collapse='')
        private$.instances[[id]] <- instance
        id
      } else {
        stop(paste('Unknown type:', type))
      }
    },

    #' @section \code{get} method:
    #'
    #' Get an instance
    #' 
    #' \describe{
    #'   \item{id}{ID of instance}
    #' }
    #' 
    #' Returns the instance
    get  = function (id) {
      instance <- private$.instances[[id]]
      if (!is.null(instance)) {
        instance
      } else {
        stop(paste('Unknown instance:', id))
      }
    },

    #' @section \code{put} method:
    #'
    #' Call a method of an instance
    #' 
    #' \describe{
    #'   \item{id}{ID of instance}
    #'   \item{method}{Name of instance method}
    #'   \item{args}{A list of of method arguments}
    #' }
    #'
    #' Returns the result of the method call
    put  = function (id, method, args = NULL) {
      instance <- private$.instances[[id]]
      if (!is.null(instance)) {
        func <- instance[[method]]
        if (!is.null(func)) {
          do.call(func, args)
        } else {
          stop(paste('Unknown method:', method))
        }
      } else {
        stop(paste('Unknown instance:', id))
      }
    },

    #' @section \code{delete} method:
    #'
    #' Delete an instance
    #' 
    #' \describe{
    #'   \item{id}{ID of the instance}
    #' }
    delete  = function (id) {
      instance <- private$.instances[[id]]
      if (!is.null(instance)) {
        private$.instances[[id]] <- NULL
      } else {
        stop(paste('Unknown instance:', id))
      }
    },

    #' @section \code{start} method:
    #'
    #' Start serving this host
    #'
    #' Currently, HTTP is the only server available
    #' for hosts. We plan to implement a `HostWebsocketServer` soon. 
    start  = function () {
      if (is.null(private$.servers[['http']])) {
        server <- HostHttpServer$new(self)
        private$.servers[['http']] <- server
        server$start()
      }
      invisible(self)
    },

    #' @section \code{stop} method:
    #'
    #' Stop serving this host. Stops all servers that are currently serving this host
    stop  = function () {
      for (name in names(private$.servers)) {
        server <- private$.servers[[name]]
        server$stop()
        private$.servers[[name]] <- NULL
      }
      invisible(self)
    },

    #' @section \code{view} method:
    #'
    #' View this host in the browser. Opens the default browser at the URL of this host
    view  = function () {
      # Difficult to test headlessly, so don't include in coverage
      # nocov start
      self$start()
      url <- private$.servers[['http']]$url
      if (Sys.info()[['sysname']] == 'Linux') {
        system(paste0('2>/dev/null 1>&2 xdg-open "', url, '"'))
      } else {
        system(paste0('open "', url, '"'))
      }
      invisible(self)
      # nocov end
    }
  ),

  active = list(
    #' @section \code{servers} method:
    #'
    #' Get a list of server names for this host. Servers are identified by the protocol shorthand
    #' e.g. `http` for `HostHttpServer` 
    servers = function () {
      names(private$.servers)
    }
  ),

  private = list(
    .servers = NULL,
    .instances = NULL
  )
)
