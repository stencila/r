#' A base for context classes to share implementation of
#' variable packing and unpacking
Context <- R6::R6Class("Context",
  public = list(

    compile = function(cell) {
      lang <- NULL
      code <- ""
      expr <- FALSE
      global <- FALSE
      inputs <- list()
      if (typeof(cell) == "character") {
        code <- cell
      } else {
        if (!is.null(cell$code)) code <- cell$code
        else if (!is.null(cell$source) & !is.null(cell$source$data)) code <- cell$source$data # legacy structure
        if (!is.null(cell$expr)) expr <- cell$expr
        if (!is.null(cell$global)) global <- cell$global
        if (!is.null(cell$inputs)) inputs <- cell$inputs
      }

      list(
        type = "cell",
        lang = lang,
        code = code,
        expr = expr,
        global = global,
        options = list(),
        inputs = inputs,
        outputs = list(),
        messages = list()
      )
    },

    pack = function (value) {
      type <- type(value)
      # Of course, the order of these if statements is important.
      # Rearrange with caution (and testing!)
      if (type == "table") {
        df <- as.data.frame(value)
        list(
          type = "table",
          data = list(
            type = "table",
            columns = ncol(df),
            rows = nrow(df),
            # Limit the amount of data actually sent
            data = head(df, n=100)
          )
        )
      } else if (type == "plot") {
        format <- "src"
        path <- tempfile(fileext = paste0(".", format))
        png(path)
        if (inherits(value, "recordedplot")) replayPlot(value)
        else print(value)
        dev.off()
        list (
          type = "image",
          src = paste0("data:image/", format, ";base64,", base64enc::base64encode(path))
        )
      } else if (type == "unknown") {
        # Unknown types serialised using `print` which may be customised
        # e.g. `print.table` is used for the results of `summary`
        content <- paste(capture.output(print(value)), collapse = "\n")
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
        packed <- from_json(packed)
      }
      # Ensure data package is a list with necessary properties
      if (!inherits(packed, "list") ) {
        stop("should be a list")
      }
      if (!"type" %in% names(packed)) {
        stop("should have field `type`")
      }

      type <- packed$type
      if (type == "array") {
        if (is.list(packed$data) && length(packed$data) == 0) vector()
        else packed$data
      } else if (type == "table") {
        do.call(data.frame, c(packed$data$data, stringsAsFactors = FALSE))
      } else {
        packed$data
      }
    },

    libraries = function (...) {
      list()
    }
  )
)
