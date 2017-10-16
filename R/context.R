#' A base for context classes to share implementation of
#' variable packing and unpacking
Context <- R6::R6Class('Context',
  public = list(
    pack = function (value) {
      type <- libcore::type(value)
      # Of course, the order of these if statements is important.
      # Rearrange with caution (and testing!)
      if (type == 'table') {
        df <- as.data.frame(value)
        list(
          type = 'table',
          data = list(
            values = df,
            columns = ncol(df),
            rows = nrow(df)
          )
        )
      } else if (type == 'plot') {
        format <- 'src'
        path <- tempfile(fileext=paste0('.', format))
        png(path)
        if (inherits(value, 'recordedplot')) replayPlot(value)
        else print(value)
        dev.off()
        list (
          type = 'image',
          src = paste0('data:image/', format, ';base64,', base64enc::base64encode(path))
        )
      } else if (type == 'unknown') {
        # Unknown types serialised using `print` which may be customised
        # e.g. `print.table` is used for the results of `summary`
        content <- paste(capture.output(print(value)), collapse = '\n')
      } else {
        list(
          type = type,
          data = value
        )
      }
    },

    unpack = function (packed) {
      # If necessary convert JSON to list
      if (inherits(packed, "character")) {
        packed <- fromJSON(packed)
      }
      # Ensure data package is a list with necessary properties
      if (!inherits(packed, "list") ) {
        stop("Package should be a list")
      }

      type <- packed$type
      if (type == 'array') {
        obj <- fromJSON(content)
        if (is.list(obj) && length(obj)==0) obj <- vector()
        obj
      } else if (type == 'table') {
        values <- packed$data$values
        do.call(data.frame, c(values, stringsAsFactors = FALSE))
      } else {
        if (is.na(packed$data)) stop(paste0('No data to unpack type "', type, '"'))
        packed$data
      }
    }
  )
)
