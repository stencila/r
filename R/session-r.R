RSession <- R6Class("RSession",
  inherit = Component,

  public = list(

    initialize = function () {
      super$initialize()
      private$.parent <- NULL
      private$.children <- list()
      env <- new.env(parent=parent.frame())
      env$self <- self
      env$parent <- private$.parent
      env$children <- private$.children
      private$.root <- env
      private$.scopes <- list(env)
    },

    execute = function(code, format = '') {
      # TODO If format is not specified then default to one based on value. Otherwise
      # attempt to force to particular format e.g plot to json?
      # `evaluate` returns a list : source code lines interleaved with output, just as if
      # you typed each line in an interactive console.
      result <- evaluate(code, envir=private$.bottom(), output_handler=execute_output_handler)
      errors <- list()
      output <- list()
      # Find any errors
      if (length(result) > 1) {
        for (line in seq(2, length(result), 2)) {
          out <- result[[line]]
          if (inherits(out, 'error')) {
            errors[[toString(line/2)]] <- out$message
          }
        }
        # Last line is the output
        last <- result[[length(result)]]
        if (inherits(last, 'error')) {
          # Ignore errors (already captured above)
        } else if (inherits(last, 'data.frame') || inherits(last, 'matrix')) {
          # Tabular data to CSV
          csv <- capture.output(write.csv(last, file=stdout(), row.names=FALSE, col.names=TRUE, quote=FALSE))
          csv <- paste(csv, collapse='\n')
          output <- list(
            format = 'csv',
            content = csv
          )
        } else if (inherits(last, 'recordedplot') || inherits(last, 'ggplot')) {
          # Plots to PNG
          path <- tempfile()
          png(path)
          if (inherits(last, 'recordedplot')) replayPlot(last)
          else print(last)
          dev.off()
          output <- list(
            format = 'png',
            content = dataURI(file=path, mime="image/png")
          )
        } else if (typeof(last) %in% c('NULL', 'logical', 'integer', 'double', 'character')) {
          # Fundamental types as JSON (atomics and vectors) with fallback to printing
          format <- 'json'
          content <- tryCatch(toJSON(last, auto_unbox = TRUE), error = identity)
          if (inherits(content, 'error')) {
            format <- 'text'
            content <- paste(capture.output(print(last)), collapse = '\n')
          }
          output <- list(
            format = format,
            content = content
          )
        } else {
          # Everthing else is best viewed using `print` which may be customised
          # e.g. `print.table` for the results of `summary`
          output <- list(
            format = 'text',
            content = paste(capture.output(print(last)), collapse = '\n')
          )
        }
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

    get = function (name) {
      get(name, envir=private$.bottom())
    },

    set = function (name, value) {
      assign(name, value, envir=private$.bottom())
    },

    spawn = function(type) {
      child <- host$open(paste0('new://session-', type))
      child$parent <- self
      private$.children[[type]] <- child
    },

    #' Get a child using a name
    child = function(name) {
      private$.children[[name]]
    },

    sibling = function(name) {
      if (private$.parent) {
        stop('This session does not have a parent, and thus, doesn\'t have siblings')
      } else {
        private$.parent$child(name)
      }
    }
  ),

  active = list(

    type = function () {
      'session-r'
    },

    parent = function (value) {
      if (missing(value)) {
        private$.parent
      } else {
        private$.parent <- value
        private$.root$parent <- private$.parent
      }
    }

  ),

  private = list(
    .parent = NULL,
    .children = NULL,
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

