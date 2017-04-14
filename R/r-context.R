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
#' context$runCode('my_var <- 42')
#' context$runCode('my_var')
#' context$callCode('my_var / 7')
#' context$callCode('x * 2', list(x=pack(2)))
#' context$callCode('plot(1,1)')
#' @export
RContext <- R6::R6Class('RContext',
  public = list(

    #' @section new():
    #'
    #' Create a new \code{RContext}
    #'
    #' \describe{
    #'   \item{global}{Scope of context is the global environment. Default \code{FALSE}}
    #'   \item{closed}{Context is not nested within the global environment. Default \code{FALSE}}
    #' }
    initialize = function (global=FALSE, closed=FALSE) {
      # Create a global environment for the context (utilised by `runCode()`)
      if (global) env <- globalenv() # Can pollute global env
      else {
        # Can't pollute global env...
        if (closed) env <- new.env(parent=baseenv()) # and can't access it
        else env <- new.env(parent=globalenv()) # but can access it
      }
      private$.global_env <- env
      # Create a function environment for the context (utilised by `callCode()`)
      # Note that this intentionally does not have access to context's global namespace
      # Ensures no leakage between runCode and callCode (either way)
      env <- new.env(parent=baseenv())
      # Create nested environments to make necessary namespaces accesible
      for (package in c('utils', 'grDevices', 'graphics', 'stats')) {
        namespace <- getNamespace(package)
        for(name in ls(namespace)) assign(name, namespace[[name]], envir=env)
      }
      private$.func_env <- env
    },

    #' @section runCode():
    #'
    #' Run R code within the context's scope
    #'
    #' \describe{
    #'   \item{code}{R code to be executed}
    #'   \item{options}{Any execution options}
    #' }
    runCode = function(code, options = list()) {
        # Do eval and process into a result
        evaluation <- evaluate(code, envir=private$.global_env, output_handler=evaluate_output_handler)
        private$.result(evaluation)
    },

    #' @section callCode():
    #'
    #' Run R code within a local function scope
    #'
    #' \describe{
    #'   \item{code}{R code to be executed}
    #'   \item{args}{A list with a data pack for each argument}
    #' }
    callCode = function(code, args = NULL) {
      # Create a local enviroment for execution
      local <- new.env(parent=private$.func_env)
      for (arg in names(args)) local[[arg]] <- unpack(args[[arg]])

      # Overide the return function so that we capture the first returned
      # value and stop execution (the `stop_on_error` below)
      value_returned <- NULL
      has_returned <- FALSE
      local[['return']] <- function (value) {
        value_returned <<- value
        has_returned <<- TRUE
        stop('Returned')
      }

      # Do eval and process into a result
      evaluation <- evaluate(code, envir=local, stop_on_error=1L, output_handler=evaluate_output_handler)
      result <- private$.result(evaluation)

      # If returned a value, use that as output
      if (has_returned) result$output <- pack(value_returned)

      result
    },

    #' @section codeDependencies():
    #'
    #' Returns an array of all variable names not declared within
    #' the piece of code. This might include global functions and variables used
    #' within the piece of code.
    #'
    #' \describe{
    #'   \item{code}{R code}
    #' }
    codeDependencies = function(code) {
      # `all.names` just parses out all variable names, it does not doo dependency analysis
      # Package `codetools` or something similar probably nees to be used
      # But see http://adv-r.had.co.nz/Expressions.html#ast-funs
      names <- all.names(parse(text=code))
      # Exclude name in base environment (which includes functions like '+', '-', 'if')
      in_base <- names %in% ls(baseenv())
      names <- names[!in_base]
      # all.names includes duplicates, so...
      unique(names)
    }

  ),

  private = list(
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
      last_value <- NULL
      value <- FALSE
      for (item in evaluation) {
        if (inherits(item, 'source')) {
          line <- line + max(1, str_count(item, '\n'))
        } else if (inherits(item, 'error')) {
          errors[[toString(line)]] <- item$message
        } else {
          last_value <- item
          value <- TRUE
        }
      }

      if (length(errors) == 0) errors <- NULL
      output <- if (value) pack(last_value) else NULL

      list(errors=errors, output=output)
    }
  )
)

# Custom output handler for the `run` and `call` methods
# Returns the value itself instead of the default which is to `print()` it
evaluate_output_handler = evaluate::new_output_handler(
  # No `visible` argument so that only visible results get converted to string
  value = function(value) {
    value
  }
)
