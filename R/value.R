#' Get the type code for a value
#'
#' @param value A R Value
#' @return Type code for value
#' @export
type <- function(value) {
  len <- length(value)
  type <- tolower(typeof(value))

  # Of course, the order of these if statements is important. Rearrange with caution (and testing!)
  if (type == 'null' || (len == 1 && type %in% c('logical', 'integer', 'double', 'character'))) {
    if (is.null(value) || is.na(value)) {
      'null'
    } else {
      switch(type,
         logical = 'boolean',
         integer = 'integer',
         double = 'float',
         character = 'string',
         type
      )
    }
  } else if (inherits(value, 'data.frame') || inherits(value, 'matrix')) {
    'table'
  } else if (inherits(value, 'recordedplot') || inherits(value, 'ggplot')) {
    # Use the special 'plot' type to identify plot values that need
    # to be converted to the standard 'image' type during `pack()`
    'plot'
  } else if (is.list(value)) {
    type <- value$type
    if (typeof(type) == 'character' && length(type) == 1) type
    else 'object'
  } else if (is.vector(value)) {
    'array'
  } else {
    'unknown'
  }
}

#' Pack a R value into a package
#'
#' @param value The R value to be packaged
#' @return A package as an R \code{list}
#' @export
pack <- function(value) {
  type_ <- type(value)
  format <- 'text'

  # Of couse, the order of these if statements is important. Rearrange with caution (and testing!)
  if (type_ == 'null') {
    content <- 'null'
  } else if (type_ == 'boolean') {
    content <- ifelse(value, 'true', 'false')
  } else if (type_ %in% c('integer', 'float', 'string')) {
    content <- toString(value)
  } else if (type_ == 'table') {
    format <- 'json'
    content <- toString(toJSON(list(
      type = 'table',
      data = lapply(as.data.frame(value), function(column) {
        if (is.ordered(column)) type <- 'ordinal'
        else if (is.factor(column)) type <- 'nominal'
        else type <- 'quantitative'
        list(
          values = column,
          type = type
        )
      })
    ), auto_unbox = TRUE))
  } else if (type_ == 'plot') {
    type_ <- 'image'
    format <- 'png'
    path <- tempfile(fileext=paste0('.', format))
    png(path)
    if (inherits(value, 'recordedplot')) replayPlot(value)
    else print(value)
    dev.off()
    content <- base64enc::base64encode(path)
  } else if (type_ == 'unknown') {
    # Unknown types serialised using `print` which may be customised
    # e.g. `print.table` is used for the results of `summary`
    content <- paste(capture.output(print(value)), collapse = '\n')
  } else {
    # Catches 'object', 'array' and custom types
    format <- 'json'
    content <- toString(toJSON(value, auto_unbox = TRUE, force = TRUE))
    if (type_ == 'object' && content == '[]') content <- '{}'
  }

  list(type=type_, format=format, content=content)
}

#' Unpack a package into a R value
#'
#' @param package The package as a \code{list} or JSON string
#' @return A R value
#' @export
unpack <- function(package) {
  # If necessary convert JSON to list
  if (inherits(package, "character")) {
    package <- fromJSON(package)
  }
  # Ensure data package is a list with necessary properties
  if (!inherits(package, "list") ) {
    stop("Package should be a list")
  }
  if (anyNA(match(c('type', 'format', 'content'), names(package)))) {
    stop("Package should have fields `type`, `format`, `content`")
  }

  type <- package$type
  format <- package$format
  content <- package$content

  if (type == 'null') {
    NULL
  } else if (type == 'boolean') {
    as.logical(content)
  } else if (type == 'integer') {
    as.integer(content)
  } else if (type == 'float') {
    as.double(content)
  } else if (type == 'string') {
    as.character(content)
  } else if (type == 'object') {
    fromJSON(content)
  } else if (type == 'array') {
    obj <- fromJSON(content)
    if (is.list(obj) && length(obj)==0) obj <- vector()
    obj
  } else if (type == 'table') {
    if (format == 'json') {
      table <- fromJSON(content)
      values <- lapply(table$data, function(x) x[['values']])
      do.call(data.frame, c(values, stringsAsFactors = FALSE))
    } else if (format == 'csv') {
      read.csv(text=content, as.is=T)
    } else {
      stop(paste0('Unable to unpack\n  type: ', type, '\n  format: ', format))
    }
  } else {
    stop(paste0('Unable to unpack\n  type: ', type, '\n  format: ', format))
  }
}
