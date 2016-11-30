DocumentProxy <- function(type, url) {
  self <- ComponentProxy(type, url)

  class(self) <- c('DocumentProxy', class(self))
  self
}
