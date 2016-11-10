Pipeline <- R6Class("Pipeline",
  public = list(

    #' The pipes that make up this pipeline
    pipes = NULL,

    initialize = function () {
      self$pipes <- list()
    }

  )
)

# Get raw data from a pipe
'[.Pipeline' <- function(pipeline, name = 'default') {
  pipeline$pull(name)
}

OutputPipeline <- R6Class("OutputPipeline",
  inherit = Pipeline,

  public = list(
    #' Push data into one of this pipeline's pipe's
    push = function (data, name = 'default') {
      self$pipes[[name]] <- toString(toJSON(data, auto_unbox = TRUE))
    }
  )
)

InputPipeline <- R6Class("InputPipeline",
  inherit = Pipeline,

  public = list(

    #' Get the raw JSON string data from one of this
    #' pipeline's pipes
    #'
    #' @param name The name of the pipe
    pull = function (name = 'default') {
      pipe <- self$pipes[[name]]
      if (is.null(pipe)) {
        # No such pipe exists
        stop(paste0('No pipe with this name exits.\n  name: ', name))
      } else {
        # Request JSON from the source session
        SessionProxy('session', pipe)$supply(name)
      }
    },

    #' Get a pipe as a string
    #'
    #' @param name The name of the pipe
    str = function (name) {
      as.character(fromJSON(self$pull(name)))
    },

    #' Get a pipe as a number
    #'
    #' @param name The name of the pipe
    num = function (name) {
      as.numeric(fromJSON(self$pull(name)))
    },

    #' Get a pipe as a list
    #'
    #' @param name The name of the pipe
    list = function (name) {
      as.list(fromJSON(self$pull(name)))
    },

    #' Join several pipes together based on matching their name
    #' Returns a list of raw string values. Will need to be
    #' transofrmed by the user
    join = function (pattern = '.*') {
      result <- list()
      for (name in names(self$pipes)) {
        if (str_detect(name, pattern)) {
          result[[name]] <- self$pull(name)
        }
      }
      result
    }
  )
)


