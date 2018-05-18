# Convert JSON to a value
from_json <- function(json) {
  fromJSON(json, simplifyDataFrame = FALSE)
}

# Convert a value to JSON
to_json <- function(value) {
  # Override jsonlite which converts empty R lists to empty JSON arrays
  if (is.list(value) && length(value) == 0) {
    "{}"
  } else {
    toString(toJSON(
      value,
      null = "null",
      na = "null",
      dataframe = "columns",
      digits = NA,
      auto_unbox = TRUE
    ))
  }
}

asJSON <- jsonlite:::asJSON

# Create a hook for conversion of R6 instances to JSON
methods::setClass("R6")
methods::setMethod("asJSON", "R6", function(x, ...) {
  members <- list()
  for (name in ls(x, sorted = FALSE)) {
    if (!is.function(x[[name]])) members[[name]] <- x[[name]]
  }
  to_json(members)
})
