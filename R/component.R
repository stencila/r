Component <- R6Class("Component",
  public = list(

    initialize = function (path = NULL) {
      path <- if (!is.null(path)) path else ''

      private$.path <- path
      if (file.exists(path)) {
        self$read(path)
      }

      count <- components$count + 1
      assign(paste0('.', count), self, envir = components)
      assign('count', count, envir = components)

    },

    path = function () {
      private$.path
    },

    read = function (path = NULL) {
      if (is.null(path) || path == '') {
        path <- private$.path
      }

      if (file.exists(path)) {
        private$.path <- path
      } else {
        stop("Filesystem path does not exist\n  path: ${path}")
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

  private = list(
    .path = NULL
  )
)

# List of component instances
# Needs to be an environment to avoid the
# "cannot change value of locked binding" error
# when trying to change a package variable
components <- new.env()
assign('count', 0, envir = components)
