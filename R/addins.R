#' Opens the file that the is currently open in the source editor in Stencila
addin_doc_open <- function() {
  # Get the path of the file currently in the source editor
  editor <- getSourceEditorContext()
  editor$path
  # If path is empty (as it is for a new document) we could revert to loading the
  # editor$contents
}

#' Inserts the current selection as a `run` cell at the cursor location in the document
addin_doc_insert <- function() {
  # Get the selected text from the currently active document (source file or console)
  active <- getActiveDocumentContext()
  active$selection[[1]]$text
}
