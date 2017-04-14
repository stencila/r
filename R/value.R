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
         logical = 'bool',
         integer = 'int',
         double = 'flt',
         character = 'str',
         type
      )
    }
  } else if (inherits(value, 'data.frame') || inherits(value, 'matrix')) {
    'tab'
  } else if (inherits(value, 'recordedplot') || inherits(value, 'ggplot')) {
    'plot'
  } else if (is.list(value)) {
    type <- value$type
    if (typeof(type) == 'character' && length(type) == 1) type
    else 'obj'
  } else if (is.vector(value)) {
    'arr'
  } else {
    'unk'
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
  } else if (type_ == 'bool') {
    content <- ifelse(value, 'true', 'false')
  } else if (type_ %in% c('int', 'flt', 'str')) {
    content <- toString(value)
  } else if (type_ == 'tab') {
    format <- 'csv'
    content <- capture.output(write.csv(value, file=stdout(), row.names=FALSE, quote=FALSE))
    content <- paste(content, collapse='\n')
  } else if (type_ == 'plot') {
    format <- 'png'
    path <- tempfile(fileext=paste0('.',format))
    png(path)
    if (inherits(value, 'recordedplot')) replayPlot(value)
    else print(value)
    dev.off()
    content <- base64enc::dataURI(file=path, mime="image/png")
  } else if (type_ == 'unk') {
    # Unknown types serialised using `print` which may be customised
    # e.g. `print.table` is used for the results of `summary`
    content <- paste(capture.output(print(value)), collapse = '\n')
  } else {
    # Catches 'obj', 'arr' and custom types
    format <- 'json'
    content <- toString(toJSON(value, auto_unbox = TRUE, force = TRUE))
    if (type_ == 'obj' && content == '[]') content <- '{}'
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
  } else if (type == 'bool') {
    as.logical(content)
  } else if (type == 'int') {
    as.integer(content)
  } else if (type == 'flt') {
    as.double(content)
  } else if (type == 'str') {
    as.character(content)
  } else if (type == 'obj') {
    fromJSON(content)
  } else if (type == 'arr') {
    obj <- fromJSON(content)
    if (is.list(obj) && length(obj)==0) obj <- vector()
    obj
  } else if (type == 'tab') {
    if (format == 'csv') {
      read.csv(text=content, as.is=T)
    } else if (format == 'tsv') {
      read.csv(text=content, sep='\t', as.is=T)
    } else {
      stop(paste0('Unable to unpack\n  type: ', type, '\n  format: ', format))
    }
  } else {
    stop(paste0('Unable to unpack\n  type: ', type, '\n  format: ', format))
  }
}
