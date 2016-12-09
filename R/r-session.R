RSession <- R6Class("RSession",
  inherit = Session,

  public = list(

    initialize = function () {
      super$initialize()

      # Root environment with `input` and `output` pipeline variables
      root <- new.env(parent=parent.frame())
      root$input <- self$input
      root$output <- self$output
      private$.root <- root

      # Put root environment to top of scope stack
      private$.scopes <- list(root)
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

    execute = function(code, args = NULL, format = '') {
      # TODO If format is not specified then default to one based on value. Otherwise
      # attempt to force to particular format e.g plot to json?
      #
      # TODO: Setup the input variable, so that it 'knows' if is will call remote session with `!output`, or
      # just use it's own `.output` value directly

      for (arg in names(args)) {
        self$set(arg, unpack(args[[arg]]))
      }

      code <- str_trim(code)
      if (nchar(code) == 0) return(list(
        errors = list(),
        output = list()
      ))

      # `evaluate` returns a list : source code lines interleaved with output, just as if
      # you typed each line in an interactive console.
      result <- evaluate(code, envir=private$.bottom(), output_handler=execute_output_handler)
      errors <- NULL
      output <- NULL
      # Find any errors
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

      list(
        errors = errors,
        output = output
      )
    },

    print = function (expr) {
      if (missing(expr)) {
        invisible(cat(str(self)))
      } else {
        evaluate(expr, envir=private$.bottom(), output_handler=print_output_handler)[[2]]
      }
    },

    list = function() {
      bottom <-private$.bottom()
      objs <- list()
      for (name in ls(envir=bottom)) {
        obj <- bottom[[name]]
        objs[[name]] <- list(
          type = class(obj),
          length = length(obj)
        )
      }
      objs
    },

    get = function (name) {
      self$pack(get(name, envir=private$.bottom()))
    },

    set = function (name, value) {
      assign(name, value, envir=private$.bottom())
    }

  ),

  active = list(

    type = function () {
      'r-session'
    }

  ),

  private = list(
    .root = NULL,
    .scopes = NULL,

    .bottom = function() {
      private$.scopes[[length(private$.scopes)]]
    }
  )
)

#' Custom output handler for the `execute` directive
execute_output_handler = evaluate::new_output_handler(
  # No `visible` argument so that only visible results get converted to string
  value = function(value) {
    value
  }
)

#' Custom output handler for the `print` directive
#' This converts a value to a plain string instead of the decorated output that you get
#' when using R's `print()` e.g "42" versus "[1] 42\n"
print_output_handler = evaluate::new_output_handler(
  # Include invisible argumet so that everything gets converted to string
  value = function(value, invisible) {
    toString(value)
  }
)

