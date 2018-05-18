#' @export
execute <- function (operation, scope = list()) {
  if (is.atomic(operation) | mode(operation) == "function") operation
  else if (operation$type == "get") {
    value <- scope[[operation$name]]
    if (is.null(value)) value <- functions[[operation$name]]
    if (is.null(value)) stop("Could not find variable: \"", operation$name, "\"")
    value
  } else if (operation$type == "call") {
    func <- execute(operation$func, scope)
    if (mode(func) == "function") {
      args <- as.list(operation$args)
      do.call(func, args)
    } else {
      stop("Value is not a function: \"", func, "\"")
    }
  } else {
    operation
  }
}
