# TODO
# - split code by semicolon and use `dbExecute` to run each line because sendQuery only
#   runs first statement (?)
# - set execute args as (a) variables that can be inserted into SQL statements e.g. using ${max_height}
#   (b) `table` data (.e.g format csv) creates a new table
# - constructor to include path - could we create a new SQliteSession by entering path to a sqlite3 file ?

#' A SQLite session
#'
#' A \code{SqliteSession} executes SQL code within an \href{SQLite}{https://sqlite.org} database. In addition to SQL statements, the 
#' \code{execute()} method support some of the \href{special commands}{https://sqlite.org/cli.html#special_commands_to_sqlite3_dot_commands_} 
#' of the SQLite command line interface (e.g. \code{tables}). Inputs to the \code{execute} method are handled differently, depending upon 
#' their type. Primitive data are stored as variables that can be inserted into SQL statements (e.g. \code{SELECT mean(height) FROM data WHERE sex=$\{sex\}}).
#' Tabular data will be saved as a new database table.
#'
#' @export
SqliteSession <- R6Class("SqliteSession",
  inherit = Session,

  public = list(

    initialize = function (path=":memory:") {
      super$initialize()

      self$db <- dbConnect(RSQLite::SQLite(), path)
    },

    #' @section \code{db} property:
    #'
    #' Connection to the SQLite database
    db = NULL,

    #' @section \code{execute()} method:
    #'
    #' Execute code within the session
    #'
    #' \describe{
    #'   \item{code}{R code to be executed}
    #'   \item{inputs}{Name of the variable}
    #' }
    execute = function(code, inputs = NULL) {
      # Create tables from tabular inputs
      table_inputs <- NULL
      for (input in names(inputs)) {
        package <- inputs[[input]]
        if (package$type == 'tab') {
          value <- unpack(package)
          dbWriteTable(self$db, input, value)
          table_inputs <- c(table_inputs, input)
        }
      }

      # Split code by semicolon and execute each statement separately
      errors <- list()
      output <- NULL
      statements <- str_split(code, ';')
      for (statement in statements) {
        if (str_sub(statement, 1, 1) == '.') {
          # Special commands
          special <- str_sub(statement, 2)
          if (special == 'tables') {
            output <- dbListTables(self$db)
          } else {
            errors[[length(errors)+1]] <- paste0('Unknown special command:', special)
          }
        } else {
          # Insert variable interpolations
          varibles <- str_match_all(statement, '\\$\\{(\\w+)\\}')[[1]][,2]
          for (variable in varibles) {
            # Get input variable and create string representation
            input <- unpack(inputs[[variable]])
            if (mode(input) == "character") input <- paste0('"', toString(input), '"')
            else if (mode(input) == "logical") input <- toString(as.integer(input))
            else input <- toString(input)
            statement <- str_replace(statement, paste0('\\$\\{', variable, '\\}'), input)
          }
          # Execute statement
          result <- tryCatch(dbGetQuery(self$db, statement), error=identity)
          if (inherits(result, 'error')) {
            errors[[length(errors)+1]] <- str_replace(toString(result), 'Error in rsqlite_send_query\\(conn@ptr, statement\\)', 'Error')
          } else {
            output <- result
          }
        }
      }

      # Remove table inputs
      for (table in table_inputs) {
        dbRemoveTable(self$db, table)
      }

      if (!is.null(output)) output <- pack(output)
      list(errors = errors, output = output)
    }

  ),

  active = list(

    type = function () {
      'sqlite-session'
    }

  )
)

