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
          db <- file.path(dir, files[matches[1]])
        }
      }
      private$.conn <- DBI::dbConnect(RSQLite::SQLite(), db)
    },

    #' @section runCode():
    #'
    #' Run R code within the context's scope
    #'
    #' \describe{
    #'   \item{code}{SQL code to be executed}
    #' }
    runCode = function(code) {
      output <- tryCatch(DBI::dbGetQuery(private$.conn, code), error=identity)
      if (inherits(output, 'error')) {
        errors <- list(list(
          line = 0,
          column = 0,
          message = output$message
        ))
        output <- NULL
      } else {
        errors <- NULL
      }
      list(errors=errors, output=pack(output))
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
      variables <- list()
      for (name in names(inputs)) {
        value <- unpack(inputs[[name]])
        if (inherits(value, 'data.frame')) {
          DBI::dbExecute(private$.conn, sprintf('DROP TABLE IF EXISTS %s', name))
          DBI::dbWriteTable(private$.conn, name, value)
        } else {
          variables[[name]] <- value
        }
      }
      matches <- str_match_all(code, '\\$\\{(\\w+)\\}')[[1]]
      if (nrow(matches) >= 1) {
        for (match in 1:nrow(matches)) {
          name <- matches[match,2]
          value <- variables[[name]]
          code <- str_replace_all(code, sprintf('\\$\\{%s\\}', name), toString(value))
        }
      }
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
