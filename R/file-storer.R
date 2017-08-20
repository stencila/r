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
    initialize = function (path = '', version=NULL) {
      private$.path = path
    },

    #' @section readFile():
    #'
    #' Read content from the file at \code{path}
    #'
    #' \describe{
    #'   \item{path}{Path within the storer}
    #' }
    readFile = function (path) {
      path <- file.path(private$.path, path)
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
      path <- file.path(private$.path, path)
      connection <- file(path, 'wt')
      # It would seem to make sense to use `writeChar` here but that
      # appears to write a binary encoded file. So using `cat`.
      cat(content, file=connection)
      close(connection)
    },

    #' @section listFiles():
    #'
    #' List files within this storer
    listFiles = function () {
      setdiff(list.files(private$.path), list.dirs(private$.path, recursive = FALSE, full.names = FALSE))
    },

    #' @section getInfo():
    #'
    #' Get information about this storer:
    #'
    #' \describe{
    #'   \item{path}{The absolute file sytem path of the storer}
    #'   \item{files}{A list of file in the storer}
    #' }
    getInfo = function () {
      list(
        path=private$.path,
        files=self$listFiles()
      )
    }
  ),

  private = list(
    .path = ''
  )
)

FileStorer$spec <- list(
  name = 'FileStorer',
  base = 'Storer',
  aliases = c('file')
)
