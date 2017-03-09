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
      errors <- NULL
      output <- NULL
      code <- str_trim(code)
      if (nchar(code) > 0) {
        # `evaluate` returns a list : source code lines interleaved with output (just as if
        # you typed each line in an interactive console). Using custom handler so that the
        # value is returned
        result <- evaluate(code, envir=private$scope, output_handler=evaluate_output_handler)

        if (length(result) > 1) {
          # Extract errors
          errors <- private$errors(result)
          # Last line is the output value
          output <- pack(result[[length(result)]])
        }
      }
      list(errors = errors, output = output)
    },

    #' @section \code{call} method:
    #'
    #' Run R code within the context's global scope (execute a code "chunk")
    #'
    #' \describe{
    #'   \item{code}{R code to be executed}
    #'   \item{inputs}{Name of the variable}
    #'   \item{package}{The data package}
    #' }
    call = function(code, inputs = NULL) {
      # Set each input variable
      for (input in names(inputs)) {
        self$set(input, inputs[[input]])
      }

      # Create a local enviroment for execution
      local <- emptyenv()

      errors <- NULL
      output <- NULL
      code <- str_trim(code)
      if (nchar(code) > 0) {
        # `evaluate` returns a list : source code lines interleaved with output (just as if
        # you typed each line in an interactive console).
        result <- evaluate(code, envir=local, output_handler=evaluate_output_handler)

        # Extract errors and output

        list(errors = errors, output = output)
      }
    }
  ),

  private = list(
    # Context's scope
    scope = NULL,

    # Extract errors from an `evaluate` result
    errors = function (result) {
      errors <- list()
      for (line in seq(2, length(result), 2)) {
        out <- result[[line]]
        if (inherits(out, 'error')) {
          errors[[toString(line/2)]] <- out$message
        }
      }
      errors
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

