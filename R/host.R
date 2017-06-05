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
      private$.id <- paste(c('r-', sample(c(letters, 0:9), 64, replace=TRUE)), collapse="")
      private$.servers <- list()
      private$.instances <- list()
    },

    #' @section user_dir():
    #'
    #' Get the current user's Stencila data directory.
    #'
    #' This is the directory that Stencila configuration settings, such as the
    #' installed Stencila hosts, and document buffers get stored.
    user_dir = function() {
      os <- tolower(Sys.info()["sysname"])
      dir <- switch(os,
        darwin = file.path(Sys.getenv("HOME"), 'Library', 'Preferences', 'Stencila'),
        linux = file.path(Sys.getenv("HOME"), '.local', 'share', 'stencila'),
        windows = file.path(Sys.getenv("APPDATA"), 'Stencila')
      )
      if (!dir.exists(dir)) dir.create(dir, recursive = T)
      dir
    },

    #' @section temp_dir():
    #'
    #' Get the current user's Stencila temporary directory
    #'
    #' This directory is used by Stencila for files such as "run files" (see below)
    temp_dir = function() {
      # Get system's temporary directory
      # Thanks to Steve Weston at https://stackoverflow.com/a/16492084/4625911
      os <- tolower(Sys.info()["sysname"])
      envs <- Sys.getenv(c('TMPDIR', 'TMP', 'TEMP'))
      useable <- which(file.info(envs)$isdir & file.access(envs, 2) == 0)
      if (length(useable) > 0)
        temp <- envs[[useable[1]]]
      else if (os == 'windows')
        temp <- Sys.getenv('R_USER')
      else
        temp <- '/tmp'
      dir <- file.path(temp, 'stencila')
      if (!dir.exists(dir)) dir.create(dir, recursive = T)
      dir
    },

    #' @section run_file():
    #'
    #' Get the path of the "run file" for this host.
    #'
    #' A run file is used to indicate that a particular host is running
    #' and allow other Stencila processes on the same machine
    #' to communicate with it. It is created by by \code{host$start()} and
    #' destroyed by \code{host$stop()}. It is placed in the machine's temporarily
    #' directory to reduce the chances of a run file being present when a host
    #' has aborted with out by \code{host$stop()} being called.
    run_file = function() {
      dir <- file.path(self$temp_dir(), 'hosts')
      if (!file.exists(dir)) dir.create(dir, recursive=TRUE)
      file.path(dir, paste0(self$id, '.json'))
    },

    #' @section new():
    #'
    #' Get the environment of this \code{Host} including the version of R
    #' and the version of installed packages.
    environ = function () {
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
        run = c(unname(Sys.which('R')), '--slave', '-e', 'stencila:::run(echo=TRUE)'),
        schemes = list(
          new = list(
            RContext = RContext$spec
          )
        )
      )
      if (complete) {
        manifest <- c(manifest, list(
          id = private$.id,
          process = Sys.getpid(),
          urls = self$urls,
          instances = names(private$.instances)
        ))
      }
      manifest
    },

    #' @section install():
    #'
    #' Install this Stencila \code{Host} on this machine.
    #'
    #' Installation of a host involves creating a file \code{r.json} inside of
    #' the user's Stencila data (see \code{user_dir}) directory which describes
    #' the capabilities of this host.
    install = function() {
      dir <- file.path(self$user_dir(), 'hosts')
      if (!file.exists(dir)) dir.create(dir, recursive=TRUE)
      cat(
        toJSON(self$manifest(complete=FALSE), pretty=TRUE, auto_unbox=TRUE),
        file=file.path(dir, 'r.json')
      )
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
    #' \describe{
    #'   \item{address}{The address to listen. Default '127.0.0.1'}
    #'   \item{port}{The port to listen on. Default 2000}
    #'   \item{quiet}{Don't print out message. Default FALSE}
    #' }
    #'
    #' Currently, HTTP is the only server available
    #' for hosts. We plan to implement a `HostWebsocketServer` soon.
    start  = function (address='127.0.0.1', port=2000, quiet=FALSE) {
      if (is.null(private$.servers[['http']])) {
        # Start HTTP server
        server <- HostHttpServer$new(self, address, port)
        private$.servers[['http']] <- server
        server$start()

        # Register as a running host by creating a run file
        file <- self$run_file()
        file.create(file)
        Sys.chmod(file, "0600")
        cat(toJSON(self$manifest(), pretty=TRUE, auto_unbox=TRUE), file=file)

        if (!quiet) cat('Host has started at', paste(self$urls, collapse=', '), '\n')
      }
      invisible(self)
    },

    #' @section stop():
    #'
    #' Stop serving this host. Stops all servers that are currently serving this host
    stop  = function (quiet=FALSE) {
      # Stop each server
      for (name in names(private$.servers)) {
        server <- private$.servers[[name]]
        server$stop()
        private$.servers[[name]] <- NULL
      }

      # Deregister as a running host
      file <- self$run_file()
      if (file.exists(file)) file.remove(file)

      if (!quiet) cat('Host has stopped\n')

      invisible(self)
    },

    #' @section run():
    #'
    #' \describe{
    #'   \item{address}{The address to listen. Default '127.0.0.1'}
    #'   \item{port}{The port to listen on. Default 2000}
    #'   \item{quiet}{Do not print status messages to the console? Default FALSE}
    #'   \item{echo}{Print the host's manifest to the console? Default FALSE}
    #' }
    #'
    #' Start serving the Stencila host and wait for connections indefinitely
    run  = function (address='127.0.0.1', port=2000, quiet=FALSE, echo=FALSE) {
      if (echo) quiet = TRUE
      self$start(address=address, port=port, quiet=quiet)
      
      if (echo) {
        cat(toJSON(self$manifest(), pretty=TRUE, auto_unbox=TRUE))
        flush.console()
      }
      
      if (!quiet) cat('Use Ctl+C (terminal) or Esc (RStudio) to stop\n')
      tryCatch(
        Sys.sleep(1e6),
        interrupt = function (condition) {
          self$stop(quiet=quiet)
        }
      )
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
    #' @section id:
    #'
    #' Get unique ID of this host
    id = function () {
      private$.id
    },

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
    .id = NULL,
    .servers = NULL,
    .instances = NULL
  )
)

#' The singleton instance of the \code{Host} class
#' @rdname host-instance
#' @export
host <- NULL
