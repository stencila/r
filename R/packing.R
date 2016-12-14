#' Pack an object into a data package
#'
#' @param object The R object to be packaged
#' @return Data package as an R list
pack <- function(object) {
  len <- length(object)
  type <- tolower(typeof(object))
  format <- NULL
  value <- NULL

  # Of couse, the order of these if statements is important. Rearrange with caution (and testing!)
  if (type == 'null' || (len == 1 && type %in% c('logical', 'integer', 'double', 'character'))) {
    # Primitive types as text
    format <- 'text'
    if (is.null(object) || is.na(object)) {
      type <- 'null'
      value <- 'null'
    } else {
      type <- switch(type,
         logical = 'bool',
         integer = 'int',
         double = 'flt',
         character = 'str',
         type
      )
      value <- toString(object)
      value <- switch (value,
         'TRUE' = 'true',
         'FALSE' = 'false',
         value
      )
    }
  } else if (inherits(object, 'data.frame') || inherits(object, 'matrix')) {
    # Tabular data to CSV
    type <- 'tab'
    format <- 'csv'
    value <- capture.output(write.csv(object, file=stdout(), row.names=FALSE, quote=FALSE))
    value <- paste(value, collapse='\n')
  } else if (inherits(object, 'recordedplot') || inherits(object, 'ggplot')) {
    # Plots to PNG
    type <- 'img'
    format <- 'png'
    path <- tempfile(fileext=paste0('.',format))
    png(path)
    if (inherits(object, 'recordedplot')) replayPlot(object)
    else print(object)
    dev.off()
    value <- dataURI(file=path, mime="image/png")
  } else if (is.list(object) || is.vector(object)) {
    # Lists and vectors to JSON objects and arrays
    if (is.list(object)) type <- 'obj'
    else type <- 'arr'
    format <- 'json'
    value <- toString(toJSON(object, auto_unbox = TRUE, force = TRUE))
    if (type == 'obj' && value == '[]') value <- '{}'
  } else {
    # Fallback to using `print` which may be customised
    # e.g. `print.table` for the results of `summary`
    # CHECK: Is this necessary?
    format <- 'text'
    content <- paste(capture.output(print(object)), collapse = '\n')
  }

  list(type=type, format=format, value=value)
}

#' Unpack a data package into an object
#'
#' @param package The data package as a \code{list} or JSON string
#' @return A R object
unpack <- function(package) {
  # If necessary convert JSON to list
  if (inherits(package, "character")) {
    package <- fromJSON(package)
  }
  # Ensure data package is a list with necessary properties
  if (!inherits(package, "list") ) {
    stop("Package should be a list")
  }
  if (anyNA(match(c('type', 'format', 'value'), names(package)))) {
    stop("Package should have fields `type`, `format`, `value`")
  }

  type <- package$type
  if (is.null(type)) type <- ''
  format <- package$format
  value <- package$value
  if (type == 'bool') {
    as.logical(value)
  } else if (type == 'int') {
    as.integer(value)
  } else if (type == 'float') {
    as.double(value)
  } else if (type == 'char' || type == 'str') {
    as.character(value)
  } else if (type == 'table') {
    if (format == 'csv') {
      read_csv(value)
    } else if (format == 'tsv') {
      read_tsv(value)
    } else {
      stop(paste0('Unable to unpack\n  type: ', type, '\n  format: ', format))
    }
  } else if (format == 'json') {
    fromJSON(value)
  } else if (type == 'fetch') {
    proxy <- SessionProxy(type='session', url=format)
    proxy$get(value)
  } else {
    stop(paste0('Unable to unpack\n  type: ', type, '\n  format: ', format))
  }
}
