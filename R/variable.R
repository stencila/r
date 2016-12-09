
pack = function(object) {
  type <- NULL
  format <- NULL
  value <- NULL
  if (inherits(object, 'data.frame') || inherits(object, 'matrix')) {
    # Tabular data to CSV
    type <- 'table'
    format <- 'csv'
    csv <- capture.output(write.csv(object, file=stdout(), row.names=FALSE, col.names=TRUE, quote=FALSE))
    value <- paste(csv, collapse='\n')
  } else if (inherits(object, 'recordedplot') || inherits(object, 'ggplot')) {
    # Plots to PNG
    path <- tempfile()
    png(path)
    if (inherits(object, 'recordedplot')) replayPlot(object)
    else print(object)
    dev.off()
    format <- 'png'
    value <- dataURI(file=path, mime="image/png")
  } else if (typeof(object) %in% c('NULL', 'logical', 'integer', 'double', 'character')) {
    # Fundamental types as JSON (atomics and vectors) with fallback to printing
    format <- 'json'
    value <- tryCatch(toString(toJSON(object, auto_unbox = TRUE)), error = identity)
    if (inherits(content, 'error')) {
      format <- 'text'
      value <- paste(capture.output(print(object)), collapse = '\n')
    }
  } else {
    # Everthing else is best viewed using `print` which may be customised
    # e.g. `print.table` for the results of `summary`
    format <- 'text'
    content <- paste(capture.output(print(object)), collapse = '\n')
  }

  list(type=type, format=format, value=value)
}

unpack = function(package) {
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
