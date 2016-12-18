#' @export
RSession <- R6Class("RSession",
  inherit = Session,

  public = list(

    initialize = function () {
      super$initialize()

      # Create scope stack
      self$scopes <- list(new.env(parent=parent.frame()))
    },

    dump = function (format = 'data', options = list()) {
      if (format == 'data') {
        data <- super$dump('data', options)
        data[['list']] <- self$list()
        data
      } else {
        super$dump(format, options)
      }
    },

    #' @section \code{scopes} property:
    #'
    #' List of this session's scopes
    scopes = NULL,

    #' @section \code{enter()} method:
    #'
    #' Exit the current scope
    enter = function () {
      self$scopes[[length(self$scopes)+1]] <- new.env(parent=self$bottom)
    },

    #' @section \code{exit()} method:
    #'
    #' Exit the current scope
    exit = function () {
      if (length(self$scopes) == 1) {
        stop('Can not exit the top scope; unbalanced enter()/exit() calls')
      }
      self$scopes[[length(self$scopes)]] <- NULL
    },

    #' @section \code{set()} method:
    #'
    #' Set a session variable from a data package
    #'
    #' \describe{
    #'   \item{name}{Name of the variable}
    #'   \item{package}{The data package}
    #' }
    set = function (name, package) {
      assign(name, unpack(package), envir=self$bottom)
      invisible(self)
    },

    #' @section \code{get()} method:
    #'
    #' Get a session variable as a data package
    #'
    #' \describe{
    #'   \item{name}{Name of the variable}
    #' }
    get = function (name) {
      pack(get(name, envir=self$bottom))
    },

    #' @section \code{execute} method:
    #'
    #' Execute code within the session
    #'
    #' \describe{
    #'   \item{code}{R code to be executed}
    #'   \item{inputs}{Name of the variable}
    #'   \item{package}{The data package}
    #' }
    execute = function(code, inputs = NULL) {
      # Set each input variable
      for (input in names(inputs)) {
        self$set(input, inputs[[input]])
      }

      code <- str_trim(code)
      if (nchar(code) == 0) {
        list(errors = NULL, output = NULL)
      } else {
        # `evaluate` returns a list : source code lines interleaved with output, just as if
        # you typed each line in an interactive console.
        result <- evaluate(code, envir=self$bottom, output_handler=execute_output_handler)
        
        # Extract errors and output
        errors <- NULL
        output <- NULL
        if (length(result) > 1) {
          for (line in seq(2, length(result), 2)) {
            out <- result[[line]]
            if (inherits(out, 'error')) {
              if (is.null(errors)) errors <- list()
              errors[[toString(line/2)]] <- out$message
            }
          }
          # Last line is the output
          output <- pack(result[[length(result)]])
        }

        list(errors = errors, output = output)
      }
    }

  ),

  active = list(

    type = function () {
      'r-session'
    },

    #' @section \code{top} property:
    #'
    #' The top scope in this session
    top = function () {
      self$scopes[[1]]
    },

    #' @section \code{bottom} property:
    #'
    #' The bottom scope in this session
    bottom = function () {
      self$scopes[[length(self$scopes)]]
    }

  )
)

# Custom output handler for the `execute` directive
execute_output_handler = evaluate::new_output_handler(
  # No `visible` argument so that only visible results get converted to string
  value = function(value) {
    value
  }
)

# Custom output handler for the `print` directive
# This converts a value to a plain string instead of the decorated output that you get
# when using R's `print()` e.g "42" versus "[1] 42\n"
print_output_handler = evaluate::new_output_handler(
  # Include invisible argumet so that everything gets converted to string
  value = function(value, invisible) {
    toString(value)
  }
)

