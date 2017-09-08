#' A file storer
#' @export
FileStorer <- R6::R6Class('FileStorer',

  public = list(

    #' @section new():
    #'
    #' Create a new \code{FileStorer}
    #'
    #' \describe{
    #'   \item{path}{Local file system path. Default \code{''}}
    #'   \item{version}{Version of storer. Currently ignored but required for compatability. Default \code{NULL}}
    #' }
    initialize = function (path = '/', version = NULL) {
      path <- suppressWarnings(normalizePath(path, winslash='/'))
      if (file.exists(path)) {
        isdir <- file.info(path)[1, 'isdir']
      } else {
        isdir <- !str_detect(path, '\\.([[:alnum:]]+)$')
      }
      private$.dir <- if (isdir) path else dirname(path)
      private$.main <- if (isdir) NULL else basename(path)
    },

    #' @section getDirectory():
    #'
    #' Get the directory path of this storer
    getDirectory = function () {
      private$.dir
    },

    #' @section getMain():
    #'
    #' Get the main file, if specified
    getMain = function () {
      private$.main
    },

    #' @section getFiles():
    #'
    #' List files within this storer
    getFiles = function () {
      I(setdiff(list.files(private$.dir), list.dirs(private$.dir, recursive = FALSE, full.names = FALSE)))
    },

    #' @section getInfo():
    #'
    #' Get information about this storer:
    #'
    #' \describe{
    #'   \item{dir}{The absolute file sytem path of the storer's directory}
    #'   \item{main}{The main file, if specified}
    #'   \item{files}{A list of file in the storer}
    #' }
    getInfo = function () {
      list(
        dir=self$getDirectory(),
        main=self$getMain(),
        files=self$getFiles()
      )
    },

    #' @section readFile():
    #'
    #' Read content from the file at \code{path}
    #'
    #' \describe{
    #'   \item{path}{Path within the storer}
    #' }
    readFile = function (path) {
      path <- file.path(private$.dir, path)
      connection <- file(path, 'rt')
      content <- readChar(connection, file.info(path)$size)
      close(connection)
      content
    },

    #' @section writeFile():
    #'
    #' Write content to the file at \code{path}
    #'
    #' \describe{
    #'   \item{path}{Path within the storer}
    #'   \item{content}{Content to write}
    #' }
    writeFile = function (path, content) {
      path <- file.path(private$.dir, path)
      connection <- file(path, 'wt')
      # It would seem to make sense to use `writeChar` here but that
      # appears to write a binary encoded file. So using `cat`.
      cat(content, file=connection)
      close(connection)
    },

    #' @section deleteFile():
    #'
    #' Delete the file at \code{path}
    #'
    #' \describe{
    #'   \item{path}{Path within the storer}
    #' }
    deleteFile = function (path) {
      path <- file.path(private$.dir, path)
      file.remove(path)
    }

  ),

  private = list(
    .dir = NULL,
    .main = NULL
  )
)

FileStorer$spec <- list(
  name = 'FileStorer',
  base = 'Storer',
  aliases = c('file')
)
