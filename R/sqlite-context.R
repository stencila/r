#' A SQLite context
#'
#' @export
SqliteContext <- R6::R6Class('SqliteContext',
  inherit = Context,
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

    # Destructor called when instance is garbage collected
    finalize = function() {
      # Prevent the warning "call dbDisconnect() when finished working with a connection"
      DBI::dbDisconnect(private$.conn)
    },

    #' @section compile():
    #'
    #' Analyse SQL code and return the names of inputs, outputs
    #' and the returned value
    #'
    #' \describe{
    #'   \item{code}{SQL code to be analysed}
    #'   \item{exprOnly}{Ensure that the code is a select expression?}
    #' }
    compile = function(code, exprOnly = FALSE) {
      inputs <- list()
      output <- NULL
      value <- FALSE
      messages <- list()

      if (exprOnly) {
        # Only SELECT statements allowed
        if (!str_detect(code, regex('^\\s*SELECT\\s+', ignore_case = TRUE))) {
          messages[[length(messages)+1]] <- list(
            line = 0,
            column = 0,
            type = 'error',
            message = 'Code must be a `SELECT` expression'
          )
        } else {
          value <- TRUE
        }
      }

      if (length(messages) == 0) {
        # Determine tabular data inputs
        match <- str_match(code, regex('SELECT\\b.+?\\bFROM\\s+(\\w+)', ignore_case = TRUE))[1,]
        if (!is.na(match[2])) {
          table <- match[2]
          # Select from `sqlite_master` table in the `main` schema therby ignoring input tables in the `temp` schema
          tables <- DBI::dbGetQuery(private$.conn, 'SELECT name FROM sqlite_master WHERE type=="table"')$name
          if (!table %in% tables) inputs <- c(inputs, table)
          value <- TRUE
        } else if (str_detect(code, regex('^\\s*SELECT\\s+', ignore_case = TRUE))) {
          value <- TRUE
        }
        # Determine other inputs (string interpolated using ${})
        inputs <- c(inputs, str_match_all(code, '\\$\\{(\\w+)\\}')[[1]][,2])
        # Is there an output?
        name <- str_match(code, regex('^\\s*(\\w+)\\s*=\\s*\\bSELECT', ignore_case = TRUE))[1,2]
        if (!is.na(name)) {
          output <- name
          value <- TRUE
        }
      }

      # Produce an error if user is attempting to 'overwrite' an input
      if (!is.null(output)) {
        if(output %in% inputs) {
          messages[[length(messages)+1]] <- list(
            line = 0,
            column = 0,
            type = 'error',
            message = paste0('Attempting to overwrite cell input "', output, '"')
          )
          output <- NULL
        }
      }

      list(
        inputs = inputs,
        output = output,
        value = value,
        messages = messages
      )
    },

    #' @section executeCode():
    #'
    #' Run SQL code
    #'
    #' \describe{
    #'   \item{code}{SQL code to be executed}
    #'   \item{inputs}{A list with a value pack for each input}
    #'   \item{exprOnly}{Ensure that the code is a simple expression?}
    #' }
    executeCode = function(code, inputs = NULL, exprOnly = FALSE) {
      analysis <- self$analyseCode(code)

      variables <- list()
      for (name in names(inputs)) {
        value <- self$unpack(inputs[[name]])
        if (inherits(value, 'data.frame')) {
          DBI::dbWriteTable(private$.conn, name, value, temporary=TRUE, overwrite=TRUE)
        } else {
          variables[[name]] <- value
        }
      }
      matches <- str_match_all(code, '\\$\\{(\\w+)\\}')[[1]]
      if (nrow(matches) >= 1) {
        for (match in 1:nrow(matches)) {
          name <- matches[match,2]
          str <- toString(variables[[name]])
          code <- str_replace_all(code, sprintf('\\$\\{%s\\}', name), str)
        }
      }

      match <- str_match(code, regex('^\\s*(\\w+)\\s*=\\s*\\b(SELECT\\b.*)$', ignore_case = TRUE))[1,]
      if (!is.na(match[3])) {
        code <- match[3]
      }

      func <- if (analysis$value) DBI::dbGetQuery else DBI::dbExecute
      value <- tryCatch(func(private$.conn, code), error=identity)
      if (inherits(value, 'error')) {
        messages <- list(list(
          line = 0,
          column = 0,
          type = "error",
          message = value$message
        ))
        value <- NULL
      } else {
        messages <- list()
      }
      list(
        inputs = analysis$inputs,
        output = analysis$output,
        value = self$pack(value),
        messages = messages
      )
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
  client = 'ContextHttpClient'
)
