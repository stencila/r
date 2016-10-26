library(DBI)

Host <- R6Class("Host",
  inherit = Component,
  public = list(

    initialize = function () {
      super$initialize()
      self$components <- list()

      private$.home = path.expand('~/.stencila')
      if (!file.exists(private$.home)) {
        dir.create(private$.home)
      }

      private$.logs = file.path(private$.home, '.logs')
      if (!file.exists(private$.logs)) {
        dir.create(private$.logs)
      }

      private$.db <- NULL

      private$.token <- paste0(sample(c(letters, paste(0:9)), 12), collapse='')
    },

    # When trying to make this a private member accessible through a getter
    # it failed to work (weirdly). So, just use a public member.
    components = list(),

    register = function(component) {
      self$components <- c(self$components, component)
    },

    deregister = function(component) {
    },

    open = function(address) {
      if (is.null(address)) return(self)

      address <- self$lengthen(address)
      parts <- self$split(address)
      scheme <- parts$scheme
      path <- parts$path
      format <- parts$format
      version <- parts$version
      if (scheme == 'new') {
        class = switch(path,
          'session-r' = RSession,
          NULL
        )
        if (!is.null(class)) {
          return(class$new())
        }
      }

      for (component in self$components) {
        if (scheme == 'id') {
          if (component$id == path) return(component)
        } else {
          if (component$address == paste0(scheme, '://', path)) return(component)
        }
      }

      #path <- self$obtain(address)
      # if (is.null(path)) {
      #   addresses <- ''#[com.address for com in self._components]
      # }
      # stop(paste0('Not able to find in-memory component\n  address: ', address, '\n  addresses: ', addresses)

      # for (class in c(Document, Sheet, Session, Context)) {
      #   if (class$know(path)) {
      #     return class$new(address, path)
      #   }
      # }

      proxy <- self$ask(address)
      if (!is.null(proxy)) {
        return(proxy)
      }

      stop(paste0('Unable to open address\n address: ', address))
    },

    obtain = function(address, version=None){
      address <- self$resolve(address)
      path <- NULL

      if (str_sub(address, 1, 6) == 'mem://') {
        return(NULL)
      } else if (str_sub(address, 1, 7) == 'file://') {
        path <- str_sub(address, 8)
        if (file.exists(path)) {
          return(path)
        } else {
          stop(paste0('Filesystem path does not exist\n  address: ', address, '\n  path: ', path))
        }
      } else {
        stop(paste0('Unhandled address\n  address: ', address))
      }

      # else if (address[:7] == 'http://' or address[:8] == 'https://') {
      #   response = requests.get(address)
      # if (response.status_code == 200) {
      #   root, extension = os.path.splitext(address)
      # if (not extension) {
      #   type = response.headers.get('Content-Type', None)
      # extension = mimetypes.guess_extension(type)
      # handle, path = tempfile.mkstemp(extension)
      # with builtins.open(path, 'w') as file:
      #   file.write(response.text.encode('utf-8'))
      # return path
      # else:
      #   raise IOError(
      #     'Unable to obtain HTTP address\n  address: %s\n  status code: %s\n  message: %s' % (
      #       address, response.status_code, response.text
      #     )
      #   )
      # else if (address[:6] == 'git://') {
      #   match = re.match('git://([\w\-\.]+)/([\w\-]+/[\w\-]+)/(.+)$', address)
      # if (match) {
      #   host = match.group(1)
      # if (host == 'stenci.la') {
      #   host_dir = ''
      # else:
      #   host_dir = host

      # repo = match.group(2)
      # repo_dir = file.path(self._home, host_dir, repo)
      # master_dir = file.path(repo_dir, 'master')
      # if (not file.exists(master_dir)) {
      #   url = 'https://%s/%s.git' % (host, repo)
      # #info('Cloning repository\n  url: %s\n  directory: %s' % (url, master_dir))
      # git.clone(url, master_dir)

      # if (version and version != 'master') {
      #   repo = Git(master_dir)
      # if (not repo.exists(version)) {
      #   #info('Updating repository\n directory: %s' % repo_dir)
      #   repo.pull()
      # if (not repo.exists(version)) {
      #   stop(paste0('Version does not exist in the repository\n  repository: %s\n  version: %s' % (repo_dir, version)))
      # version_dir = file.path(repo_dir, version)
      # repo.export(version_dir, version)
      # else:
      #   version_dir = master_dir

      # file = match.group(3)
      # path = file.path(version_dir, file)
      # if (not file.exists(path)) {
      #   raise IOError('Path does not exist\n  path: %s' % path)
      # return path
      # else:
      #   stop(paste0('Unable to determine Git repository URL from address\n  address: %s' % address))
    },

    discover = function () {
      self$peers <- list()
      for (port in seq(2000, 3000, 10)) {
        response <- tryCatch(
          POST(
            sprintf('http://127.0.0.1:%d/!hello', port),
            content_type_json(),
            timeout(0.1),
            body = toJSON(self$manifest())
          ),
          error = identity
        )
        if (inherits(response, 'error')) {
          # print(response)
        } else if (response$status_code == 200) {
          text <- content(response, 'text', encoding = 'UTF-8')
          manifest <- fromJSON(text)
          self$peers <- c(self$peers, list(manifest))
        }
      }
    },

    ask = function (address) {
      if (length(self$peers) == 0) {
        warning('This host has no peers. Try `host$discover()` to find some.')
      } else {
        for (peer in self$peers) {
          if (!is.null(peer$url)) {
            response <- GET(paste0(peer$url, '/', address))
            if (response$status_code == 200) {
              text <- content(response, 'text', encoding='UTF-8')
              if (nchar(text)) {
                data <- fromJSON(text)
                type <- data$type
                url <- data$url
                if (str_sub(type, 1, 7) == 'session') {
                  return(SessionProxy(type, url))
                } else if (type == 'document') {
                  return(DocumentProxy(type, url))
                } else {
                  stop(paste0('Unhandled component type\n  type: ', type))
                }
              }
            }
          }
        }
        warning(paste0('No peers are able to open address\n  address: ', address))
      }
      NULL
    },

    get = function(address=NULL, format='html') {
      if (!is.null(address)) {
        component <- self$open(address)
        component$get(format)
      } else {
        if (format == 'data') {
          list(
            'stencila' = self$._id,
            'package' = 'py',
            'version' = toString(packageVersion("stencila"))#,
            # TODO 'components' = [(com.type, com.address) for com in self._components],
            # TODO 'servers' = dict([(type, server.origin) for type, server in self._servers.items()]),
          )
        } else{
          str_replace(Host.html, '\\{\\{data\\}\\}', toString(toJSON(self$get(NULL, format='data'))))
        }
      }
    },

    servers = list(),

    serve = function(on=TRUE){
      server <- HttpServer$new(self)
      server$serve()
      self$servers[['http']] <- server
      dbSendStatement(self$db, sprintf('INSERT INTO hosts VALUES ("%s", "%s")', self$id, self$url))
      self$url
    },

    startup = function () {
      self$serve()
      self$discover()
      invisible(self)
    },

    shutdown = function () {
      self$serve(FALSE)
      invisible(self)
    },

    view = function(component=NULL){
      self$serve()
      url <- paste0(self$url(component), '?token=', private$.token)
      if (Sys.info()[['sysname']] == 'Linux') {
        system(paste('2>/dev/null 1>&2 xdg-open "', url, '"'))
      } else {
        system(paste('open "', url, '"'))
      }
    },

    peers = list(),

    manifest = function() {
      list(
        stencila = TRUE,
        package = 'r',
        version = version,
        id = self$id,
        url = self$url,
        schemes = c('new', 'id', 'file'),
        types = c('session-r', ''), # Additional empty string to prevent unboxing in conversion to JSON
        formats = c('', '')
      )
    },

    hello = function(manifest) {
      # TODO replace existing entry for this peer with this new manifest
      # based on id
      self$peers <- c(self$peers, list(manifest))
      self$manifest()
    }

  ),

  active = list(

    db = function() {
      if (is.null(private$.db)) {
        private$.db <- dbConnect(RSQLite::SQLite(), file.path(private$.home, 'stencila.db3'))
        dbSendStatement(private$.db, "CREATE TABLE IF NOT EXISTS hosts (
          id TEXT,
          url TEXT
        )")
      }
      private$.db
    },

    token = function () {
      private$.token
    },

    url = function () {
      server <- self$servers[['http']]
      if (!is.null(server)) {
        server$url
      } else {
        NULL
      }
    }
  ),

  private = list(
    .home = NULL,
    .logs = NULL,
    .db = NULL,
    .token = NULL
  )
)
