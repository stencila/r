# This file is used to define necessary import and importFrom statements in the package's
# NAMESPACE file using roxygen2. Since those imports are global to the entire package (unlike
# for example, Python's module local import staements), for purposes of DRY they may as well be
# all together here in none file.

#' @importFrom base64enc dataURI
#' @import DBI
#' @importFrom evaluate evaluate
#' @importFrom grDevices dev.off png replayPlot
#' @import httr
#' @import httpuv
#' @import jsonlite
#' @import methods
#' @importFrom R6 R6Class
#' @importFrom readr read_csv read_tsv
#' @import RSQLite
#' @import stringr
#' @importFrom tibble data_frame
#' @import tools
#' @importFrom utils capture.output write.csv
#' @importFrom xtable xtable
NULL

# Get the version 
version <- tryCatch(toString(packageVersion("stencila")), error = '0.0.0')

# Hooks for namespace events
# See https://stat.ethz.ch/R-manual/R-devel/library/base/html/ns-hooks.html

# Called when namespace is loaded
.onLoad <- function (libname, pkgname) {
  # This appears to be better than instantiating 'globally'
  # which causes issues with bindings
  host <<- Host$new()
}
