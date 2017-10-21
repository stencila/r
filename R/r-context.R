#' A R context
#'
#' An execution context for R code
#'
#' In Stencila, a "context" is the thing that executes code for a particular programming language.
#' This is the context for R.
#' It implements the \code{Context} API so that it can talk to other parts of the platform,
#' including contexts for other languages, Documents, and Sheets.
#'
#' @format \code{R6Class}.
#' @examples
#' context <- RContext$new()
#'
#' # Assign a variable within the context
#' context$executeCode('my_var <- 42')
#'
#' # Get the variable as an output value
#' context$executeCode('my_var')
#'
#' # The variable is NOT available in `callCode`
#' context$executeCode('my_var')$messages[[1]]$message
#'
#' # Returned output value can include plots
#' context$executeCode('plot(1,1)')$value
#' @export
RContext <- R6::R6Class('RContext',
  inherit = Context,
  public = list(

    #' @section new():
    #'
    #' Create a new \code{RContext}
    #'
    #' Currently the parameter \code{closed} defaults to \code{FALSE} so that you can use
    #' \code{library(somepackage)} to make a package available in subsequent calls to
    #' \code{runCode} or \code{callCode}. In the future, it would be good to have a better machanism for that.
    #'
    #' \describe{
    #'   \item{local}{Context can not assign to the global environment. Default \code{TRUE}}
    #'   \item{closed}{Context can not read from the global environment. Default \code{FALSE}}
    #' }
    initialize = function (dir=NULL, local=TRUE, closed=FALSE) {
      # Set the working directory
      private$.dir <- dir
      if (!is.null(dir)) {
        setwd(dir)
      }

      # Create a 'packages' environment that contains all the functions available to the context
      if (closed) packages_env <- new.env(parent=baseenv())
      else packages_env <- new.env(parent=globalenv())
      # Assign names in package namespaces to the package environment
      for (package in RContext$packages) {
        namespace <- getNamespace(package)
        for(name in ls(namespace)) {
          assign(name, get(name, envir=namespace), envir=packages_env)
        }
      }

      # Create a global environment for the context (utilised by `runCode()`)
      if (local) env <- new.env(parent=packages_env) # Can't pollute global env
      else env <- globalenv() # Can pollute global env
      private$.global_env <- env

      # Create a function environment for the context (utilised by `callCode()`)
      # Note that this intentionally does not have access to context's global namespace
      # Ensures no leakage between runCode and callCode (either way)
      env <- new.env(parent=packages_env)
      private$.func_env <- env

      # Global variable names that should be ignored when determining inputs
      # in `analyseCode()`
      private$.globals <- ls(getNamespace('base'))
    },

    #' @section analyseCode():
    #'
    #' Analyse R code and return the names of inputs, outputs
    #' and the implicitly returned vaue expression
    #'
    #' \describe{
    #'   \item{code}{R code to be analysed}
    #'   \item{exprOnly}{Ensure that the code is a simple expression?}
    #' }
    analyseCode = function(code, exprOnly = FALSE) {
      inputs <- character()
      output <- NULL
      messages <- NULL

      # Parse the code
      ast <- tryCatch(parse(text=code), error=identity)
      if (inherits(ast, 'error')) {
        messages <- c(messages, ast)
      }

      # Is an expression an assignment?
      is.assignment <- function(expr) {
        if (is.call(expr)) {
          op <- expr[[1]]
          if (op == '<-' | op == '=') return(TRUE)
        }
        FALSE
      }

      if (is.null(messages) & exprOnly) {
        # Check for single, simple expression
        fail = FALSE
        if (length(ast) != 1) fail = TRUE
        else {
          expr <- ast[[1]]
          # Dissallow assignments
          if (is.assignment(expr)) fail <- TRUE
        }
        if (fail) {
          messages <- c(messages, list(
            line = 0,
            column = 0,
            type = 'error',
            message = 'Code is not a single, simple expression'
          ))
        }
      }

      if (is.null(messages)) {
        # Determine which names are declared and which are used
        declared <- NULL
        for (expr in ast) {
          if (is.assignment(expr)) {
            if (is.name(expr[[2]])) declared <- c(declared, as.character(expr[[2]]))
          }
          used <- all.vars(expr)
          undeclared <- !(used %in% declared) & !(used %in% private$.globals)
          if (any(undeclared)) inputs <- c(inputs, used[undeclared])
        }

        if (length(ast) > 0) {
          last <- ast[[length(ast)]]
          if (is.assignment(last)) {
            if (is.name(last[[2]])) {
              output <- as.character(last[[2]])
            }
          } else if (is.name(last)) {
            output <- as.character(last)
          }
        }
      }

      list(
        inputs = inputs,
        output = output,
        messages = messages
      )
    },

    #' @section executeCode():
    #'
    #' Run R code within the context's scope
    #'
    #' \describe{
    #'   \item{code}{R code to be executed}
    #'   \item{inputs}{A list with a data pack for each input}
    #'   \item{exprOnly}{Ensure that the code is a simple expression?}
    #' }
    executeCode = function(code, inputs = list(), exprOnly = FALSE) {
        for (input in names(inputs)) private$.global_env[[input]] <- self$unpack(inputs[[input]])
        # Do eval and process into a result
        evaluation <- evaluate::evaluate(
          code,
          envir=private$.global_env,
          output_handler=evaluate_output_handler
        )
        private$.result(evaluation)
    },

    getFunction = function(name){
      # Use the Rd based documentation, if any, for the function
      # Get the right help file and convert it into a list
      # The "deparse(substitute(..))" converts the expression into a character
      # and is what help does internally if it is not provided with
      # a character string anyway.
      # Thanks to Jeroen at
      #   http://stackoverflow.com/questions/8918753/r-help-page-as-object
      rd_files <- help(name)
      if (length(rd_files) > 0) {
          # Currently, taking the first found file
          rd <- utils:::.getHelpFile(rd_files[1])
          names(rd) <- substring(sapply(rd, attr, "Rd_tag"), 2)

          temp_args <- rd$arguments
          rd$arguments <- NULL
          docs <- lapply(rd, unlist)
          docs <- lapply(docs, paste, collapse = "")

          temp_args <- temp_args[sapply(temp_args, attr, "Rd_tag") == "\\item"]
          temp_args <- lapply(temp_args, lapply, paste, collapse = "")
          temp_args <- lapply(temp_args, "names<-", c("arg", "description"))
          docs$arguments <- temp_args
      } else {
        docs <- list()
      }

      # Extract properties from the docs into a specification
      spec <- xml_new_root("function")
      xml_add_child(spec, "language", "r")
      xml_add_child(spec, "name", docs[["name"]])
      xml_add_child(spec, "title", docs[["title"]])
      xml_add_child(spec, "summary", docs[["description"]])
      xml_add_child(spec, "description", docs[["details"]])
      xml_add_child(spec, "params", docs[["arguments"]])
      xml_add_child(spec, "return", docs[["value"]])

      # Rd examples area single single of code so put it
      # into <examples><example><usage>
      examples <- xml_add_child(spec, "examples")
      example <- xml_add_child(examples, "example")
      xml_add_child(example, "usage", docs[["examples"]])

      # Jump through hoops to get XML string...
      raw <- xml_serialize(spec, NULL)
      raw <- raw[raw != 0]
      char <- rawToChar(raw)
      str_sub(char, str_locate(char, '<function>')[1], str_locate(char, '</function>')[2])
    }
  ),

  private = list(
    # Context's working directory
    .dir = NULL,

    # Global variable names
    .globals = NULL,

    # Context's global scope
    .global_env = NULL,

    # Context's function scope
    .func_env = NULL,

    # Extract errors and the last value from an `evaluate` result
    # Note that not all source items will have a value (e.g. an emptyline)
    # Also, sometimes lines are sometimes groupd together so we need to count lines
    .result = function (evaluation) {

      line <- 0
      errors <- list()
      has_value <- FALSE
      last_value <- NULL
      for (item in evaluation) {
        if (inherits(item, 'source')) {
          line <- line + max(1, str_count(item, '\n'))
        } else if (inherits(item, 'error')) {
          if(item$message != '~return~') {
            errors[[length(errors)+1]] <- list(
              line = line,
              column = 0,
              type = "error",
              message = item$message
            )
          }
        } else {
          last_value <- item
          has_value <- TRUE
        }
      }

      if (has_value) {
        # Errors can occur in conversion of values e.g. ggplots
        # so they must be caught here
        output <- tryCatch(self$pack(last_value), error=identity)
        if (inherits(output, 'error')) {
          errors[[length(errors)+1]] <- list(
            line = 0,
            column = 0,
            type = "error",
            message = output$message
          )
          output <- NULL
        }
      } else {
        output <- NULL
      }

      if (length(errors) == 0) errors <- NULL

      list(
        value = output,
        messages = errors
      )
    }
  )
)

# Specification of an RContext (used in host manifest)
RContext$spec <- list(
  name = 'RContext',
  base = 'Context',
  aliases = c('r', 'R')
)

# List of packages made available within a RContext
RContext$packages <-  c(
  # Usual base packages (type `search()` in a naked R session)
  'methods', 'datasets', 'utils', 'grDevices', 'graphics', 'stats',
  # Core tidyverse packages http://tidyverse.org/
  'ggplot2', 'tibble', 'tidyr', 'readr', 'purrr', 'dplyr',
  # Other useful tidyverse packages
  'stringr'
)

# Custom output handler for the `run` and `call` methods
# Returns the value itself instead of the default which is to `print()` it
evaluate_output_handler = evaluate::new_output_handler(
  # No `visible` argument so that only visible results get converted to string
  value = function(value) {
    value
  }
)
