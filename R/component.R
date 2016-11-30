host <- NULL

Component <- R6Class("Component",
  public = list(

    initialize = function (address=NULL, path=NULL) {
      private$.id <- paste(sprintf('%x', sample(0:255,size=32,replace=TRUE)), collapse='')
      private$.address <- paste0('id://', private$.id)
      private$.path <- NULL
      private$.meta <- list()

      if (!is.null(host)) host$register(self)
    },

    lengthen = function(address=NULL) {
      if (is.null(address)) address <- self$address

      c1 = str_sub(address, 1, 1)
      if (!anyNA(str_match(address, '^[a-z]+://'))) {
        address
      } else if (c1 == '+'){
        paste0('new://', str_sub(address, 2))
      } else if (c1 == '*'){
        paste0('id://', str_sub(address, 2))
      } else if (c1 == '.' || c1 == '/' || c1 == '~'){
        paste0('file://', suppressWarnings(normalizePath(address)))
      } else if (str_sub(address, 1, 3) == 'bb/'){
        paste0('git://bitbucket.org/', str_sub(address, 4))
      } else if (str_sub(address, 1, 3) == 'gh/'){
        paste0('git://github.com/', str_sub(address, 4))
      } else if (str_sub(address, 1, 3) == 'gl/'){
        paste0('git://gitlab.com/', str_sub(address, 4))
      } else {
        paste0('git://stenci.la/', address)
      }
    },

    shorten = function(address=NULL) {
      if (is.null(address)) address <- self$address

      if (str_sub(address, 1, 6) == 'new://'){
        paste0('+', str_sub(address, 7))
      } else if (str_sub(address, 1, 5) == 'id://'){
        paste0('*', str_sub(address, 6))
      } else if (str_sub(address, 1, 7) == 'file://'){
        address
      } else if (str_sub(address, 1, 7) == 'http://' || str_sub(address, 1, 9)== 'https://'){
        address
      } else if (str_sub(address, 1, 20) == 'git://bitbucket.org/'){
        paste0('bb/', str_sub(address, 21))
      } else if (str_sub(address, 1, 17) == 'git://github.com/'){
        paste0('gh/', str_sub(address, 18))
      } else if (str_sub(address, 1, 17) == 'git://gitlab.com/'){
        paste0('gl/', str_sub(address, 18))
      } else if (str_sub(address, 1, 16) == 'git://stenci.la/'){
        str_sub(address, 17)
      } else {
        stop(paste0('Unable to shorten address\n address: ', address))
      }
    },

    split = function(address=NULL) {
      if (is.null(address)) address <- self$address

      address <- self$lengthen(address)
      matches <- str_match(address, '([a-z]+)://([\\w\\-\\./]+)(@([\\w\\-\\.]+))?')
      if (!is.na(matches[1, 1])) {
        return(list(
          scheme = matches[1, 2],
          path = matches[1, 3],
          format = tools::file_ext(matches[1, 3]),
          version = matches[1, 5]
        ))
      } else {
        stop(paste0('Unable to split address\n address: ', address))
      }
    },

    dump = function (format = 'data', options = list()) {
      if (format == 'data') {
        list(
          type = self$type,
          id = self$id,
          address = self$address,
          url = self$url
        )
      } else if (format == 'json') {
        toJSON(self$dump('data'), auto_unbox=TRUE)
      } else {
        stop(paste0('Unhandled format\n  format: ', format))
      }
    },

    read = function (path = NULL) {
      if (is.null(path) || path == '') {
        path <- private$.path
      }

      if (file.exists(if (is.null(path)) '' else path)) {
        private$.path <- path
      } else {
        stop(paste0("Filesystem path does not exist\n  path: ", path))
      }

      private$.path
    },

    write  = function (path = NULL) {
      if (is.null(path) || path == '') {
        path <- private$.path
      }

      dir <- if (tools::file_ext(path) == '') path else dirname(path)
      if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }

      private$.path <- path

      private$.path
    },

    show = function (format='html') {

      if (format == 'json') {
        self$dump('json')
      } else {
        paste0("<!DOCTYPE html>
        <html>
            <head>
                <meta name=\"generator\" content=\"stencila-r-", version, ">
                <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
                <link rel=\"stylesheet\" type=\"text/css\" href=\"/web/", self$kind, ".min.css\">
            </head>
            <body>
                <script id=\"data\" data-format=\"json\" type=\"application/json\">", self$dump('json'), "</script>
                <script src=\"/web/", self$kind, ".min.js\"></script>
            </body>
        </html>")
      }
    },

    view = function() {
      instance$view(self)
    }

  ),

  active = list(

    type = function () {
      tolower(class(self)[1])
    },

    kind = function () {
      self$type
    },

    id = function () {
      private$.id
    },

    address = function () {
      private$.address
    },

    path = function () {
      private$.path
    },

    url = function () {
      paste0(host$url, '/', self$shorten())
    }

  ),

  private = list(
    .id = NULL,
    .address = NULL,
    .path = NULL,
    .meta = NULL
  )
)

setClass('Component')

# Create a hook for conversion of components to JSON
asJSON <- jsonlite:::asJSON
setMethod("asJSON", "Component", function(x, ...) {
  x$dump('json')
})

