type <- function(value) {
  len <- length(value)
  type <- tolower(typeof(value))

  # Of course, the order of these if statements is important.
  # Rearrange with caution (and testing!)
  if (
    type == "null" ||
    (len == 1 && type %in% c("logical", "integer", "double", "character"))
  ) {
    if (is.null(value) || is.na(value)) {
      "null"
    } else {
      switch(type,
         logical = "boolean",
         integer = "integer",
         double = "number",
         character = "string",
         type
      )
    }
  } else if (inherits(value, "data.frame") || inherits(value, "matrix")) {
    "table"
  } else if (inherits(value, "recordedplot") || inherits(value, "ggplot")) {
    # Use the special "plot" type to identify plot values that need
    # to be converted to the standard "image" type during `pack()`
    "plot"
  } else if (is.list(value)) {
    type <- value$type
    if (typeof(type) == "character" && length(type) == 1) type
    else "object"
  } else if (is.vector(value)) {
    "array"
  } else {
    "unknown"
  }
}
