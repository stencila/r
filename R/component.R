Component <- R6Class("Component",
  public = list(

    initialize = function (address=NULL, path=NULL) {

      if (!is.null(address)) {
        private$.address = instance$resolve(address)
        if (!is.null(path)) {
          private$.path = path
        } else {
          private$.path = instance$obtain(private$.address)
          self$read()
        }
      } else {
        private$.address = paste0('mem://', paste0(sample(c(letters, paste(0:9)), 12), collapse=''))
        private$.path = NULL
      }

    },

    read = function (path = NULL) {
      if (is.null(path) || path == '') {
        path <- private$.path
      }

      if (file.exists(if (is.null(path)) '' else path)) {
        private$.path <- path
      } else {
        stop(paste0("Filesystem path does not exist\n  path: ", path))
      }

      private$.path
    },

    write  = function (path = NULL) {
      if (is.null(path) || path == '') {
        path <- private$.path
      }

      dir <- if (tools::file_ext(path) == '') path else dirname(path)
      if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }

      private$.path <- path

      private$.path
    }

  ),

  active = list(

    address = function () {
      private$.address
    },

    path = function () {
      private$.path
    }

  ),

  private = list(
    .address = NULL,
    .path = NULL
  )
)

# List of component instances
# Needs to be an environment to avoid the
# "cannot change value of locked binding" error
# when trying to change a package variable
components <- new.env()
assign('count', 0, envir = components)
