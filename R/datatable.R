#' @export
Datatable <- R6Class("Datatable",
  inherit = Component,

  public = list(

    data = NULL,

    initialize = function () {
      super$initialize()

      self$data = data.frame()
    },

    dump = function (format = 'data', options = list()) {
      if (format == 'data') {
        data <- super$dump('data', options)
        data[['data']] <- head(self$data, 1000)
        data
      } else if (format == 'html') {
        # The junk textConnection prevents printing to stdout
        junk <- textConnection('junk', 'w')
        html <- print(xtable(self$data), type='html', comment=FALSE, file=junk)
        close.connection(junk)
        html
      } else {
        super$dump(format, options)
      }
    },

    read = function (path, format) {
      if (format == 'csv') self$data <- read.csv(path)
      else stop(paste('Unhandled format:', format))
    },

    show = function (format='html') {
        # This is a temporary implementation.
        # Need a better way for derived components to either embed html or json, perhaps
        # return a tuple of (format, content) from call to dump('show')
        paste0("<!DOCTYPE html>
          <html>
              <head>
                  <meta name=\"generator\" content=\"stencila-r-", version, ">
                  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
                  <link rel=\"stylesheet\" type=\"text/css\" href=\"/web/", self$kind, ".min.css\">
              </head>
              <body>
                  <main id=\"data\" data-format=\"html\">", self$dump('html'), "</main>
                  <script src=\"/web/", self$kind, ".min.js\"></script>
              </body>
          </html>")
    }

  ),

  active = list(

    type = function () {
      'datatable'
    },

    kind = function () {
      'datatable'
    }

  ),

  private = list(
  )
)

