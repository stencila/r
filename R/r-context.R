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
#' context$execute('my_var <- 42')
#'
#' # Get the variable as an output value
#' context$execute('my_var')
#'
#' # Returned output value can include plots
#' context$execute('plot(1,1)')$value
#' @export
RContext <- R6::R6Class("RContext",
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
      if (closed) packages_env <- new.env(parent = baseenv())
      else packages_env <- new.env(parent = globalenv())
      # Assign names in package namespaces to the package environment
      for (package in RContext$packages) {
        namespace <- getNamespace(package)
        for (name in ls(namespace)) {
          assign(name, get(name, envir = namespace), envir = packages_env)
        }
      }

      # Create a global environment for the context (utilised by `runCode()`)
      if (local) env <- new.env(parent = packages_env) # Can't pollute global env
      else env <- globalenv() # Can pollute global env
      private$.global_env <- env

      # Create a function environment for the context (utilised by `callCode()`)
      # Note that this intentionally does not have access to context's global namespace
      # Ensures no leakage between runCode and callCode (either way)
      env <- new.env(parent = packages_env)
      private$.func_env <- env

      # Global variable names that should be ignored when determining inputs
      # in `analyseCode()`
      private$.globals <- ls(getNamespace("base"))
    },

    #' @section compile():
    #'
    #' Analyse R code and return the names of inputs, outputs
    #' and the implicitly returned vaue expression
    #'
    #' \describe{
    #'   \item{code}{R code to be analysed}
    #'   \item{exprOnly}{Ensure that the code is a simple expression?}
    #' }
    compile = function(cell) {
      code <- cell$source$data
      exprOnly <- cell$expr

      inputs <- list()
      output <- NULL
      messages <- list()

      # Parse the code
      ast <- tryCatch(parse(text = code), error = identity)
      if (inherits(ast, "error")) {
        messages[[length(messages) + 1]] <- ast
      }

      # Is an expression an assignment?
      is.assignment <- function(expr) {
        if (is.call(expr)) {
          op <- expr[[1]]
          if (op == "<-" | op == "=") return(TRUE)
        }
        FALSE
      }

      if (length(messages) == 0 & exprOnly) {
        # Check for single, simple expression
        fail <- FALSE
        if (length(ast) != 1) fail <- TRUE
        else {
          expr <- ast[[1]]
          # Dissallow assignments
          if (is.assignment(expr)) fail <- TRUE
        }
        if (fail) {
          messages[[length(messages) + 1]] <- list(
            line = 0,
            column = 0,
            type = "error",
            message = "Code is not a single, simple expression"
          )
        }
      }

      if (length(messages) == 0) {
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

      # Ensure no circular dependency i.e. output is not in inputs
      # (This can happen if a user types a variable into a cell
      # just because they want to see it's value)
      if (!is.null(output)) {
        if (output %in% inputs) {
          messages[[length(messages) + 1]] <- list(
            line = 0,
            column = 0,
            type = "warning",
            message = "Ignoring attempt to use a cell input \"x\" as a cell output"
          )
          output <- NULL
        }
      }

      if (!is.null(output)) outputs <- list(list(name = output))
      else outputs <- list()

      list(
        inputs = lapply(inputs, function(item) list(name = item)),
        outputs = outputs,
        messages = messages
      )
    },

    #' @section execute():
    #'
    #' Run R code within the context's scope
    #'
    #' \describe{
    #'   \item{code}{R code to be executed}
    #'   \item{inputs}{A list with a data pack for each input}
    #'   \item{exprOnly}{Ensure that the code is a simple expression?}
    #' }
    execute = function(cell) {
        for (input in cell$inputs) private$.global_env[[input$name]] <- self$unpack(input$value)

        # Do eval and process into a result
        code <- cell$source$data
        evaluation <- evaluate::evaluate(
          code,
          envir = private$.global_env,
          output_handler = evaluate_output_handler
        )
        result <- private$.result(evaluation)

        # Need to ensure any output is in value
        outputs <- self$compile(cell)$outputs
        if (length(outputs)) {
          value <- get(outputs[[1]]$name, envir = private$.global_env)
          outputs[[1]]$value <- self$pack(value)
        } else {
          outputs <- list(list(value = result$value))
        }

        list(
          outputs = outputs,
          messages = result$messages
        )
    },

    getLibraries = function(){
      xml <- lapply(ls(stencila:::functions_xml), function(name) get(name, env = functions_xml))
      list(
        local = paste0("<functions>", paste0(xml, collapse = ""), "</functions>")
      )
    },

    callFunction = function(library, name, args, namedArgs){
      # At present we still need to unpack args and namedArgs
      # but in the future this might be handled by execute itself.
      argValues <- lapply(args, self$unpack)
      namedArgValues <- lapply(args, self$unpack)
      # Use `execute` to actually call the function
      result <- execute(list(
        type = "call",
        func = list(type = "get", name = name),
        args = argValues,
        namedArgs = namedArgValues
      ))
      # Pack it up
      list(
        messages = list(),
        value = self$pack(result)
      )
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
      messages <- list()
      has_value <- FALSE
      last_value <- NULL
      for (item in evaluation) {
        if (inherits(item, "source")) {
          line <- line + max(1, str_count(item, "\n"))
        } else if (inherits(item, "error")) {
          if (item$message != "~return~") {
            messages[[length(messages) + 1]] <- list(
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
        output <- tryCatch(self$pack(last_value), error = identity)
        if (inherits(output, "error")) {
          messages[[length(messages) + 1]] <- list(
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

      list(
        value = output,
        messages = messages
      )
    }
  )
)

# Specification of an RContext (used in host manifest)
RContext$spec <- list(
  name = "RContext",
  client = "ContextHttpClient"
)

# List of packages made available within a RContext
RContext$packages <-  c(
  # Usual base packages (type `search()` in a naked R session)
  "methods", "datasets", "utils", "grDevices", "graphics", "stats",
  # Core tidyverse packages http://tidyverse.org/
  "ggplot2", "tibble", "tidyr", "readr", "purrr", "dplyr",
  # Other useful tidyverse packages
  "stringr"
)

# Custom output handler for the `run` and `call` methods
# Returns the value itself instead of the default which is to `print()` it
evaluate_output_handler <- evaluate::new_output_handler(
  # No `visible` argument so that only visible results get converted to string
  value = function(value) {
    value
  }
)
