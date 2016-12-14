#' Stencila home directory
home = path.expand('~/.stencila')

#' The abstract base class for all Stencila components
#' @importFrom R6 R6Class
#' @name Component
Component <- R6Class("Component",
  public = list(

    # Initialize this component
    #
    # @param address The address for this component
    # @param address The path for this component
    initialize = function (address=NULL, path=NULL) {
      private$.id <- paste(sprintf('%x', sample(0:255,size=32,replace=TRUE)), collapse='')

      if (!is.null(address)) private$.address <- self$long(address)
      else private$.address <- paste0('name://', substr(private$.id, 1, 5), '-', self$type)

      if (!is.null(path)) private$.path <- path
      else private$.path <- NULL

      private$.meta <- list()

      if (!is.null(host)) host$register(self)
    },

    long = function(address=NULL) {
      if (is.null(address)) address <- private$.address

      c1 = str_sub(address, 1, 1)
      if (!anyNA(str_match(address, '^[a-z]+://'))) {
        address
      } else if (c1 == '+'){
        paste0('new://', str_sub(address, 2))
      } else if (c1 == '*'){
        paste0('name://', str_sub(address, 2))
      } else if (c1 == '.' || c1 == '/' || c1 == '~'){
        paste0('file://', suppressWarnings(normalizePath(address)))
      } else {
        match <- str_match(address, '^([a-z]+)(:/?/?)(.+)$')
        if (!anyNA(match)) {
          alias <- match[1, 2]
          path <- match[1, 4]
          if (alias %in% c('id', 'http', 'https')) {
            paste0(alias, '://', path)
          } else if (alias == 'file') {
            # Only arrive here with `file:/foo` since with
            # `file:` with two or more slashes is already "long"
            paste0('file:///', path)
          } else if (alias == 'bb') {
            paste0('git://bitbucket.org/', path)
          } else if (alias == 'gh') {
            paste0('git://github.com/', path)
          } else if (alias == 'gl') {
            paste0('git://gitlab.com/', path)
          } else {
            stop(paste0('Unknown scheme alias.\n alias: ', alias))
          }
        } else {
          paste0('st://', address)
        }
      }
    },

    short = function(address=NULL) {
      if (is.null(address)) address <- self$address

      if (str_sub(address, 1, 6) == 'new://'){
        paste0('+', str_sub(address, 7))
      } else if (str_sub(address, 1, 7) == 'name://'){
        paste0('*', str_sub(address, 8))
      } else if (str_sub(address, 1, 7) == 'file://'){
        address
      } else if (str_sub(address, 1, 7) == 'http://' || str_sub(address, 1, 8)== 'https://'){
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
        matches <- str_match(address, '([a-z]+)://(.+)$')
        paste0(matches[1,2], ':', matches[1,3])
      }
    },

    split = function(address=NULL) {
      if (is.null(address)) address <- self$address

      address <- self$long(address)
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
          kind = self$kind,
          id = self$id,
          address = self$address,
          short = self$short(),
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
      host$view(self)
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
      paste0(host$url, '/', self$short())
    }

  ),

  private = list(
    .id = NULL,
    .address = NULL,
    .path = NULL,
    .meta = NULL
  )
)

methods::setClass('Component')

# Create a hook for conversion of components to JSON
asJSON <- jsonlite:::asJSON
methods::setMethod("asJSON", "Component", function(x, ...) {
  x$dump('json')
})

