SessionProxy <- function(type, url) {
  self <- ComponentProxy(type, url)

  self[['get']] <- function(name) {
    self[['.call']]('get', name)
  }

  self[['set']] <- function(name, value) {
    self[['.call']]('set', name, value)
  }

  self[['execute']] <- function(code) {
    self[['.call']]('execute', code)
  }

  self[['print']] <- function(expr) {
    self[['.call']]('print', expr)
  }

  class(self) <- c('SessionProxy', class(self))
  self
}
