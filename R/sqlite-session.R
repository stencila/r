# TODO
# - split code by semicolon and use `dbExecute` to run each line because sendQuery only
#   runs first statement (?)
# - set execute args as (a) variables that can be inserted into SQL statements e.g. using ${max_height}
#   (b) `table` data (.e.g format csv) creates a new table
# - constructor to include path - could we create a new SQliteSession by entering path to a sqlite3 file ?

#' @export
SqliteSession <- R6Class("SqliteSession",
  inherit = Session,

  public = list(

    initialize = function () {
      super$initialize()

      private$.db <- dbConnect(RSQLite::SQLite(), ":memory:")
    },

    execute = function(code, args = NULL, format = '') {
      result <- tryCatch(dbSendQuery(private$.db, code), error=identity)
      if (inherits(result, 'error')) {
        error <- toString(result)
        error <- str_replace(error, 'Error in rsqlite_send_query\\(conn@ptr, statement\\)', 'Error')
        errors <- list("0" = error)
        output <- NULL
      } else {
        errors <- NULL
        data <- dbFetch(result)
        output <- self$pack(data)
      }

      list(
        errors = errors,
        output = output
      )
    }

  ),

  active = list(

    type = function () {
      'sqlite-session'
    }

  ),

  private = list(
    .db = NULL
  )
)

