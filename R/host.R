# List of types that hosts supports
#' @include r-context.R
TYPES <- list(
  RContext = RContext
)

#' A Host
#'
#' Hosts allows you to remotely create, get, run methods of, and delete instances of various types.
#' The types can be thought of a "services" provided by the host e.g. `RContext`, `FileSystemStorer`
#'
#' The API of a host is similar to that of a HTTP server. It's methods names
#' (e.g. `post`, `get`) are similar to HTTP methods (e.g. `POST`, `GET`) but
#' the sematics sometimes differ (e.g. a host's `put()` method is used to call an
#' instance method)
#'
#' A host's methods are exposed by `HostHttpServer` and `HostWebsocketServer`.
#' Those other classes are responsible for tasks associated with their communication
#' protocol (e.g. serialising and deserialising objects).
#'
#' This is a singleton class. There should only ever be one `Host` in memory in each process
#' (although, for purposes of testing, this is not enforced)
#'
#' @format \code{R6Class}.
#' @examples
#' host$servers
#' host$start()
#' host$servers
#' host$stop()
Host <- R6::R6Class("Host",
  public = list(

    #' @section new():
    #'
    #' Create a new \code{Host}
    initialize = function () {
      private$.servers <- list()
      private$.instances <- list()
    },

    #' @section new():
    #'
    #' Get the environment of this \code{Host}
    environment = function () {
      # R
      env <- with(R.version, list(
        version = paste(major, minor, sep='.'),
        codename = nickname,
        date = paste(year, month, day, sep='-'),
        platform = platform
      ))
      # Installed packages and their versions in order of library paths
      # to prevent duplicates caused by the same package being in multiple libraries
      packages <- list()
      for(library in .libPaths()) {
        library_packages <- installed.packages(library)[, c(1, 3)]
        if (nrow(library_packages)) {
          for (row in 1:nrow(library_packages)) {
            name <- library_packages[row, 1]
            if (!(name %in% names(packages))) {
              version <- library_packages[row, 2]
              packages[[name]] <- version
            }
          }
        }
      }
      env[['packages']] <- packages[sort(names(packages))]
      env
    },

    #' @section manifest():
    #'
    #' Get a manifest for this host
    #'
    #' The manifest describes the host and it's capabilities. It is used
    #' by peer hosts to determine which "types" this host provides and
    #' which "instances" have already been instantiated.
    manifest = function (complete=TRUE) {
      manifest <- list(
        stencila = list(
          package = 'r',
          version = version
        ),
        run = c(unname(Sys.which('R')), '--slave', '-e', 'stencila:::run()'),
        schemes = list(
          new = list(
            RContext = RContext$spec
          )
        )
      )
      if (complete) {
        manifest <- c(manifest, list(
          urls = self$urls,
          instances = names(private$.instances),
          environment = self$environment()
        ))
      }
      manifest
    },

    #' @section post():
    #'
    #' Create a new instance of a type
    #'
    #' \describe{
    #'   \item{type}{Type of new instance}
    #'   \item{name}{Name of new instance}
    #'   \item{options}{Options to be passed to type constructor}
    #'   \item{return}{Address of the newly created instance}
    #' }
    post = function (type, name = NULL, options = list()) {
      Class <- TYPES[[type]]
      if (!is.null(Class)) {
        instance <- Class$new()
        if (is.null(name)) {
          name <- paste(sample(c(letters, 0:9), 10), collapse='')
        }
        address <- paste0('name://', name)
        private$.instances[[address]] <- instance
        address
      } else {
        stop(paste('Unknown type:', type))
      }
    },

    #' @section get():
    #'
    #' Get an instance
    #'
    #' \describe{
    #'   \item{id}{ID of instance}
    #'   \item{return}{The instance}
    #' }
    get  = function (id) {
      instance <- private$.instances[[id]]
      if (!is.null(instance)) {
        instance
      } else {
        stop(paste('Unknown instance:', id))
      }
    },

    #' @section put():
    #'
    #' Call a method of an instance
    #'
    #' \describe{
    #'   \item{id}{ID of instance}
    #'   \item{method}{Name of instance method}
    #'   \item{args}{A list of of method arguments}
    #'   \item{return}{The result of the method call}
    #' }
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

    #' @section delete():
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

    #' @section start():
    #'
    #' Start serving this host
    #'
    #'
    #' \describe{
    #'   \item{quiet}{Don't print out message. Default FALSE}
    #' }
    #'
    #' Currently, HTTP is the only server available
    #' for hosts. We plan to implement a `HostWebsocketServer` soon.
    start  = function (quiet=FALSE) {
      if (is.null(private$.servers[['http']])) {
        server <- HostHttpServer$new(self)
        private$.servers[['http']] <- server
        server$start()
        if (!quiet) cat('Host is served at:', paste(self$urls, collapse=', '), '\n')
      }
      invisible(self)
    },

    #' @section stop():
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

    #' @section view():
    #'
    #' View this host in the browser. Opens the default browser at the URL of this host
    view  = function (external=FALSE) {
      # Difficult to test headlessly, so don't include in coverage
      # nocov start
      self$start()
      url <- private$.servers[['http']]$url
      # See if there is a `viewer` option (defined by RStudio if we are in RStudio)
      viewer <- getOption('viewer')
      if (is.null(viewer) || external) {
        # Use builtin function to open the URL in a new browser window/tab
        utils::browseURL(url)
      } else {
        # Use the `rstudioapi` function to view in a pane
        # Arbitrarily large height to produce max height while still maintaining
        # the visibility of other panes above or below.
        viewer(url, height = 5000)
      }
      invisible(self)
      # nocov end
    }
  ),

  active = list(
    #' @section servers:
    #'
    #' Get a list of server names for this host. Servers are identified by the protocol shorthand
    #' e.g. `http` for `HostHttpServer`
    servers = function () {
      names(private$.servers)
    },

    #' @section urls:
    #'
    #' Get a list of URLs for this host
    urls = function () {
      sapply(private$.servers, function (server) server$url)
    }
  ),

  private = list(
    .servers = NULL,
    .instances = NULL
  )
)

#' The singleton instance of the \code{Host} class
#' @rdname host-instance
#' @export
host <- NULL
