#' Opens the file that is currently open in the source editor in Stencila
addin_open <- function() {
  # Get the path of the file currently in the source editor
  editor <- rstudioapi::getSourceEditorContext()
  path <- editor$path
  # If path is empty (as it is for a new document) we could revert to loading the
  # editor$contents but currently error
  if (nchar(path)==0) stop('Error when attempting to open in Stencila: no file currently active', call.=FALSE)
  # Normalise the file path so it works with the `file://` address scheme
  path <- normalizePath(path, winslash = '/')
  # Open it!
  host$open(paste0('file://', path))
}

#' Register the function in the source editor that is currently open
addin_register <- function() {
  # Get the path of the file currently in the source editor
  editor <- rstudioapi::getSourceEditorContext()
  path <- editor$path
  # If path is empty (as it is for a new document) we could revert to loading the
  # editor$contents but currently error
  if (nchar(path)==0) stop('Error when attempting to open in Stencila: no file currently active', call.=FALSE)
  # Normalise the file path
  path <- normalizePath(path, winslash = '/')
  # Open it!
  function_register(path)
}
