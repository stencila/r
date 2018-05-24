#' A R context
#'
#' An execution context for R code
#'
#' In Stencila, a "context" is the thing that executes code for a particular programming language.
#' This is the context for R.
#' It implements the \code{Context} API so that it can talk to other parts of the platform,
#' including contexts for other languages, Documents, and Sheets.
#'
#' @format \code{R6Class}
#' @export
RContext <- R6::R6Class("RContext",
  inherit = Context,
  public = list(

    #' @section new():
    #'
    #' Create a new \code{RContext}
    #'
    #' Currently the parameter \code{closed} defaults to \code{FALSE} so that you can use
    #' \code{library(somepackage)} to make a package available in subsequent calls to
    #' \code{runCode} or \code{callCode}. In the future, it would be good to have a better machanism for that.
    #'
    #' \describe{
    #'   \item{local}{Context can not assign to the global environment. Default \code{TRUE}}
    #'   \item{closed}{Context can not read from the global environment. Default \code{FALSE}}
    #' }
    initialize = function (dir=NULL, local=TRUE, closed=FALSE) {
      # Set the working directory
      private$.dir <- dir
      if (!is.null(dir)) {
        setwd(dir)
      }

      private$.variables <- new.env(parent=emptyenv())
    },

    #' @section compile():
    #'
    #' Analyse R code and return the names of inputs, outputs
    #' and the implicitly returned vaue expression
    #'
    #' \describe{
    #'   \item{code}{R code to be analysed}
    #'   \item{exprOnly}{Ensure that the code is a simple expression?}
    #' }
    compile = function(cell) {
      cell <- super$compile(cell)

      # Ensure this is an R cell
      if (!is.null(cell$lang)) {
        if (cell$lang != "r") {
          cell$messages[[length(cell$messages) + 1]] <- list(
            type = "error",
            message = "Cell code must be R code"
          )
          return(cell)
        }
      } else cell$lang <- "r"

      if (cell$code == "") return(cell)

      # Parse the code and catch any syntax errors
      ast <- tryCatch(parse(text = cell$code), error = identity)
      if (inherits(ast, "error")) {
        match <- str_match(ast$message, "<text>:(\\d):(\\d):(.+)")
        column <- as.integer(match[, 2])
        line <- as.integer(match[, 3])
        message <- match[, 4]
        cell$messages[[length(cell$messages) + 1]] <- list(
          type = "error",
          message = message,
          line = line,
          column = column
        )
        return(cell)
      }

      # Is an expression an assignment?
      is.assignment <- function(expr) {
        if (is.call(expr)) {
          op <- expr[[1]]
          if (op == "<-" | op == "=") return(TRUE)
        }
        FALSE
      }

      if (cell$expr) {
        # Check for single, simple expression
        fail <- FALSE
        if (length(ast) != 1) fail <- TRUE
        else {
          expr <- ast[[1]]
          # Dissallow assignments
          if (is.assignment(expr)) fail <- TRUE
        }
        if (fail) {
          cell$messages[[length(cell$messages) + 1]] <- list(
            type = "error",
            message = "Code is not a single, simple expression"
          )
          return(cell)
        }
      }

      # Determine inputs (variables that are global to the cell's code)
      expr <- tryCatch(eval(parse(text = paste("substitute({", cell$code, "})"))), error = identity)
      if (inherits(ast, "error")) {
        global_vars <- globals::globalsOf(expr, mustExist = F)
        global_wheres <- attr(global_vars, "where")
        inputs <- names(global_wheres)[sapply(global_wheres, is.null)]
      } else {
        inputs <- list()
      }

      # Determine output name
      output <- NULL
      last <- ast[[length(ast)]]
      if (is.assignment(last)) {
        if (is.name(last[[2]])) {
          output <- as.character(last[[2]])
        }
      }

      # Ensure no circular dependency i.e. output is not in inputs
      # (This can happen if a user types a variable into a cell
      # just because they want to see it's value)
      if (length(inputs) & !is.null(output)) {
        if (output %in% inputs) output <- NULL
      }

      # Create array of named inputs
      existing <- sapply(cell$inputs, function(input) input$name)
      for (input in inputs) {
        if (!(input %in% existing)) {
          cell$inputs[[length(cell$inputs) + 1]] <- list(name = input)
        }
      }

      # Create array on outputs
      if (!is.null(output)) cell$outputs <- list(list(name = output))
      else cell$outputs <- list()

      cell
    },

    #' @section execute():
    #'
    #' Run R code within the context's scope
    #'
    #' \describe{
    #'   \item{code}{R code to be executed}
    #'   \item{inputs}{A list with a data pack for each input}
    #'   \item{exprOnly}{Ensure that the code is a simple expression?}
    #' }
    execute = function(cell) {
      cell <- self$compile(cell)

      env <- new.env(parent = globalenv())
      for (input in cell$inputs) {
        name <- input$name
        value <- input$value
        if (name %in% ls(private$.variables)) {
          value <- get(name, envir = private$.variables)
        } else {
          value <- self$unpack(value)
        }
        env[[input$name]] <- value
      }

      # Do eval and process into a result
      evaluation <- evaluate::evaluate(
        cell$code,
        envir = env,
        output_handler = evaluate_output_handler
      )

      # Extract errors and the last value from an `evaluate` result
      # Note that not all source items will have a value (e.g. an emptyline)
      # Also, sometimes lines are sometimes groupd together so we need to count lines
      line <- 0
      messages <- list()
      value <- NULL
      has_value <- FALSE
      for (item in evaluation) {
        if (inherits(item, "source")) {
          line <- line + max(1, str_count(item, "\n"))
        } else if (inherits(item, "error")) {
          cell$messages[[length(cell$messages) + 1]] <- list(
            type = "error",
            message = item$message,
            line = line,
            column = 0
          )
        } else {
          value <- item
          has_value <- TRUE
        }
      }

      if (!has_value & length(cell$outputs)) {
        # If the last statement was an assignment then grab that variable
        name <- cell$outputs[[1]]$name
        if (!is.null(name)) {
          value <- get(name, envir = env)
          has_value <- TRUE
        }
      }

      if (has_value) {
        # Errors can occur in conversion of values e.g. ggplots
        # so they must be caught here
        packed <- tryCatch(self$pack(value), error = identity)
        if (inherits(packed, "error")) {
          cell$messages[[length(cell$messages) + 1]] <- list(
            line = 0,
            column = 0,
            type = "error",
            message = packed$message
          )
          return(cell)
        }

        if (length(cell$outputs)) {
          output <- cell$outputs[[1]]
          if (!is.null(output$name)) {
            private$.variables[[output$name]] <- value
          }
          cell$outputs[[1]]$value <- packed
        } else {
          cell$outputs <- list(list(
            value = packed
          ))
        }
      }

      cell
    },

    #nolint start
    callFunction = function(library, name, args, namedArgs){
      # At present we still need to unpack args and namedArgs
      # but in the future this might be handled by execute itself.
      argValues <- lapply(args, self$unpack)
      namedArgValues <- lapply(args, self$unpack)
      # Use `execute` to actually call the function
      result <- execute(list(
        type = "call",
        func = list(type = "get", name = name),
        args = argValues,
        namedArgs = namedArgValues
      ))
      # Pack it up
      list(
        messages = list(),
        value = self$pack(result)
      )
    }
    #nolint end
  ),

  private = list(
    # Context's working directory
    .dir = NULL,

    # Variables that reside in this context
    .variables = NULL
  )
)

# Specification of an RContext (used in host manifest)
RContext$spec <- list(
  name = "RContext",
  client = "ContextHttpClient"
)

# Custom output handler for the `run` and `call` methods
# Returns the value itself instead of the default which is to `print()` it
evaluate_output_handler <- evaluate::new_output_handler(
  # No `visible` argument so that only visible results get converted to string
  value = function(value) {
    value
  }
)
