#' A SQLite context
#'
#' @export
SqliteContext <- R6::R6Class('SqliteContext',
  public = list(

    #' @section new():
    #'
    #' Create a new \code{SqliteContext}
    #'
    #' \describe{
    #'   \item{dir}{Working directory. Default \code{NULL}}
    #' }
    initialize = function (dir=NULL) {
      # Set the working directory and connection
      private$.dir <- dir
      db <- ':memory:'
      if (!is.null(dir)) {
        files <- list.files(dir)
        matches <- grep('(.sqlite)|(.sqlite3)|(.db)|(.db3)$', files)
        if (length(matches) > 0) {
          db <- file.path(dir, files[matches[0]])
        }
      }
      private$.conn <- dbConnect(RSQLite::SQLite(), db)
    },

    #' @section runCode():
    #'
    #' Run R code within the context's scope
    #'
    #' \describe{
    #'   \item{code}{SQL code to be executed}
    #' }
    runCode = function(code) {
      dbGetQuery(private$.conn, code)
    },

    #' @section callCode():
    #'
    #' Run R code within a local function scope
    #'
    #' \describe{
    #'   \item{code}{SQL code to be executed}
    #'   \item{inputs}{A list with a value pack for each input}
    #' }
    callCode = function(code, inputs = NULL) {
      self$runCode(code)
    }

  ),

  private = list(
    # Context's working directory
    .dir = NULL,
    # Context's database connection
    .conn = NULL
  )
)

SqliteContext$spec <- list(
  name = 'SqliteContext',
  base = 'Context',
  aliases = c('sql', 'sqlite')
)
