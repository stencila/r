DocumentProxy <- function(type, url) {
  self <- ComponentProxy(type, url)

  self[['show']] <- function(format='html') {
    self[['.call']]('show', format)
  }

  class(self) <- c('DocumentProxy', class(self))
  self
}
