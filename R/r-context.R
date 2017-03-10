#' A R context
#'
#' Implements the Stencila `Context` API.
#'
#' @importFrom R6 R6Class
#' @name RContext
#' @export
RContext <- R6Class('RContext',
  public = list(

    #' @section \code{new} method:
    #'
    #' Create a new \code{RContext}
    #'
    #' \describe{
    #'   \item{global}{Scope of context is the global environment. Default \code{FALSE}}
    #'   \item{closed}{Context is not nested within the global environment. Default \code{FALSE}}
    #' }
    initialize = function (global=FALSE, closed=FALSE) {
      if (global) env <- globalenv() # Can pollute global env
      else {
        # Can't pollute global env...
        if (closed) env <- new.env(parent=baseenv()) # and can't access it
        else env <- new.env(parent=globalenv()) # but can access it
      }
      private$scope <- env
    },

    #' @section \code{run} method:
    #'
    #' Run R code within the context's scope (execute a code "chunk")
    #'
    #' \describe{
    #'   \item{code}{R code to be executed}
    #'   \item{options}{Any execution options}
    #' }
    run = function(code, options = list()) {
        # Do eval and process into a result
        evaluation <- evaluate(code, envir=private$scope, output_handler=evaluate_output_handler)
        private$result(evaluation)
    },

    #' @section \code{call} method:
    #'
    #' Run R code within the context's global scope (execute a code "chunk")
    #'
    #' \describe{
    #'   \item{code}{R code to be executed}
    #'   \item{args}{A list with a data pack for each argument}
    #' }
    call = function(code, args = NULL) {
      # Create a local enviroment for execution
      local <- new.env(parent=baseenv())
      for (arg in names(args)) local[[arg]] <- unpack(args[[arg]])

      # Overide the return function so that we capture the first returned
      # value and stop exeution (the `stop_on_error` below)
      returned <- NULL
      has_returned <- FALSE
      local[['return']] <- function (value) {
        returned <<- value
        has_returned <<- TRUE
        stop('Returned')
      }

      # Do eval and process into a result
      evaluation <- evaluate(code, envir=local, stop_on_error=1L, output_handler=evaluate_output_handler)
      result <- private$result(evaluation)

      # If returned a value, use that as output
      if (has_returned) result$output <- pack(returned)

      result
    },

    #' @section \code{depends} method:
    #'
    #' Returns an array of all variable names not declared within
    #' the piece of code. This might include global functions and variables used
    #' within the piece of code.
    #'
    #' \describe{
    #'   \item{code}{R code}
    #' }
    depends = function(code) {
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
    # Context's scope
    scope = NULL,

    # Extract errors and the last value from an `evaluate` result
    # Note that not all source items will have a value (e.g. an emptyline)
    # Also, sometimes lines are sometimes groupd together so we need to count lines
    result = function (evaluation) {
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
