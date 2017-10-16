#' A R context
#'
#' An execution context for R code
#'
#' In Stencila, a "context" is the thing that executes code for a particular programming language.
#' This is the context for R.
#' It implements the \code{Context} API so that it can talk to other parts of the platform,
#' including contexts for other languages, Documents, and Sheets.
#'
#' @format \code{R6Class}.
#' @examples
#' context <- RContext$new()
#'
#' # Assign a variable within the context
#' context$runCode('my_var <- 42')
#'
#' # Get the variable as an output value
#' context$runCode('my_var')
#'
#' # The variable is NOT available in `callCode`
#' context$callCode('my_var')$errors[[1]]$message
#'
#' # Intead, you can pass input values
#' context$callCode('x * y', list(x=pack(6), y=pack(7)))
#'
#' # Returned output value can include plots
#' context$callCode('plot(1,1)')$output
#' @export
RContext <- R6::R6Class('RContext',
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

      # Create a 'packages' environment that contains all the functions available to the context
      if (closed) packages_env <- new.env(parent=baseenv())
      else packages_env <- new.env(parent=globalenv())
      # Assign names in package namespaces to the package environment
      for (package in RContext$packages) {
        namespace <- getNamespace(package)
        for(name in ls(namespace)) {
          assign(name, get(name, envir=namespace), envir=packages_env)
        }
      }

      # Create a global environment for the context (utilised by `runCode()`)
      if (local) env <- new.env(parent=packages_env) # Can't pollute global env
      else env <- globalenv() # Can pollute global env
      private$.global_env <- env

      # Create a function environment for the context (utilised by `callCode()`)
      # Note that this intentionally does not have access to context's global namespace
      # Ensures no leakage between runCode and callCode (either way)
      env <- new.env(parent=packages_env)
      private$.func_env <- env

      # Global variable names that should be ignored when determining inputs
      # in `analyseCode()`
      private$.globals <- ls(getNamespace('base'))
    },

    #' @section analyseCode():
    #'
    #' Analyse R code and return the names of inputs, outputs
    #' and the implicitly returned vaue expression
    #'
    #' \describe{
    #'   \item{code}{R code to be analysed}
    #'   \item{exprOnly}{Ensure that the code is a simple expression?}
    #' }
    analyseCode = function(code, exprOnly = FALSE) {
      inputs <- character()
      output <- NULL
      messages <- NULL

      # Parse the code
      ast <- tryCatch(parse(text=code), error=identity)
      if (inherits(ast, 'error')) {
        messages <- c(messages, ast)
      }

      # Is an expression an assignment?
      is.assignment <- function(expr) {
        if (is.call(expr)) {
          op <- expr[[1]]
          if (op == '<-' | op == '=') return(TRUE)
        }
        FALSE
      }

      if (is.null(messages) & exprOnly) {
        # Check for single, simple expression
        fail = FALSE
        if (length(ast) != 1) fail = TRUE
        else {
          expr <- ast[[1]]
          # Dissallow assignments
          if (is.assignment(expr)) fail <- TRUE
        }
        if (fail) {
          messages <- c(messages, list(
            line = 0,
            column = 0,
            type = 'error',
            message = 'Code is not a single, simple expression'
          ))
        }
      }

      if (is.null(messages)) {
        # Determine which names are declared and which are used
        declared <- NULL
        for (expr in ast) {
          if (is.assignment(expr)) {
            if (is.name(expr[[2]])) declared <- c(declared, as.character(expr[[2]]))
          }
          used <- all.vars(expr)
          undeclared <- !(used %in% declared) & !(used %in% private$.globals)
          if (any(undeclared)) inputs <- c(inputs, used[undeclared])
        }

        if (length(ast) > 0) {
          last <- ast[[length(ast)]]
          if (is.assignment(last)) {
            if (is.name(last[[2]])) {
              output <- as.character(last[[2]])
            }
          } else if (is.name(last)) {
            output <- as.character(last)
          }
        }
      }

      list(
        inputs = inputs,
        output = output,
        messages = messages
      )
    },

    #' @section executeCode():
    #'
    #' Run R code within the context's scope
    #'
    #' \describe{
    #'   \item{code}{R code to be executed}
    #'   \item{inputs}{A list with a data pack for each input}
    #'   \item{exprOnly}{Ensure that the code is a simple expression?}
    #' }
    executeCode = function(code, inputs = list(), exprOnly = FALSE) {
        for (input in names(inputs)) private$.global_env[[input]] <- self$unpack(inputs[[input]])
        # Do eval and process into a result
        evaluation <- evaluate::evaluate(
          code,
          envir=private$.global_env,
          output_handler=evaluate_output_handler
        )
        private$.result(evaluation)
    },

    #' @section callCode():
    #'
    #' Run R code within a local function scope
    #'
    #' \describe{
    #'   \item{code}{R code to be executed}
    #'   \item{inputs}{A list with a data pack for each input}
    #'   \item{isolated}{Is the call isolated from the context's global environment}
    #' }
    callCode = function(code, inputs = NULL, isolated = FALSE) {
      # Create a local enviroment for execution
      parent <- if (isolated) private$.func_env  else private$.global_env
      local <- new.env(parent=parent)


      # Overide the return function so that we capture the first returned
      # value and stop execution (the `stop_on_error` below)
      value_returned <- NULL
      has_returned <- FALSE
      local[['return']] <- function (value) {
        value_returned <<- value
        has_returned <<- TRUE
        stop('~return~')
      }

      # Do eval and process into a result
      evaluation <- evaluate::evaluate(code, envir=local, stop_on_error=1L, output_handler=evaluate_output_handler)
      result <- private$.result(evaluation)

      # If returned a value, use that as output
      if (has_returned) result$output <- pack(value_returned)

      result
    }

  ),

  private = list(
    # Context's working directory
    .dir = NULL,

    # Global variable names
    .globals = NULL,

    # Context's global scope
    .global_env = NULL,

    # Context's function scope
    .func_env = NULL,

    # Extract errors and the last value from an `evaluate` result
    # Note that not all source items will have a value (e.g. an emptyline)
    # Also, sometimes lines are sometimes groupd together so we need to count lines
    .result = function (evaluation) {

      line <- 0
      errors <- list()
      has_value <- FALSE
      last_value <- NULL
      for (item in evaluation) {
        if (inherits(item, 'source')) {
          line <- line + max(1, str_count(item, '\n'))
        } else if (inherits(item, 'error')) {
          if(item$message != '~return~') {
            errors[[length(errors)+1]] <- list(
              line = line,
              column = 0,
              type = "error",
              message = item$message
            )
          }
        } else {
          last_value <- item
          has_value <- TRUE
        }
      }

      if (has_value) {
        # Errors can occur in conversion of values e.g. ggplots
        # so they must be caught here
        output <- tryCatch(self$pack(last_value), error=identity)
        if (inherits(output, 'error')) {
          errors[[length(errors)+1]] <- list(
            line = 0,
            column = 0,
            type = "error",
            message = output$message
          )
          output <- NULL
        }
      } else {
        output <- NULL
      }

      if (length(errors) == 0) errors <- NULL

      list(
        value = output,
        messages = errors
      )
    }
  )
)

# Specification of an RContext (used in host manifest)
RContext$spec <- list(
  name = 'RContext',
  base = 'Context',
  aliases = c('r', 'R')
)

# List of packages made available within a RContext
RContext$packages <-  c(
  # Usual base packages (type `search()` in a naked R session)
  'methods', 'datasets', 'utils', 'grDevices', 'graphics', 'stats',
  # Core tidyverse packages http://tidyverse.org/
  'ggplot2', 'tibble', 'tidyr', 'readr', 'purrr', 'dplyr',
  # Other useful tidyverse packages
  'stringr'
)

# Custom output handler for the `run` and `call` methods
# Returns the value itself instead of the default which is to `print()` it
evaluate_output_handler = evaluate::new_output_handler(
  # No `visible` argument so that only visible results get converted to string
  value = function(value) {
    value
  }
)
