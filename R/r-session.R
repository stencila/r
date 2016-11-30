RSession <- R6Class("RSession",
  inherit = Component,

  public = list(

    input = NULL,
    output = NULL,

    initialize = function () {
      super$initialize()

      # Create pipelines
      self$input <- InputPipeline$new()
      self$output <- OutputPipeline$new()

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

    execute = function(code, pipes = list(), format = '') {
      # TODO If format is not specified then default to one based on value. Otherwise
      # attempt to force to particular format e.g plot to json?
      #
      # TODO: Setup the input variable, so that it 'knows' if is will call remote session with `!output`, or
      # just use it's own `.output` value directly

      # Set the input pipes
      self$input$pipes <- pipes

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
          content <- tryCatch(toString(toJSON(last, auto_unbox = TRUE)), error = identity)
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

      if (length(errors) == 0) {
        errors <- NULL
      }

      list(
        errors = errors,
        output = output,
        pipes = I(names(self$output$pipes)) # I() prevents unboxing if only one pipe name
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
      get(name, envir=private$.bottom())
    },

    set = function (name, value) {
      assign(name, value, envir=private$.bottom())
    },

    supply = function(name) {
      self$output$pipes[[name]]
    }

  ),

  active = list(

    type = function () {
      'r-session'
    },

    kind = function () {
      'session'
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

