# Global package variables for storing registered functions
# and their XML specifications.
# Environments, instead of lists, so that they can be assigned
# to (causes a "cannot change value of locked binding" error)
functions <- new.env(parent = emptyenv())
functions_xml <- new.env(parent = emptyenv())

#' Register a function
#'
#' @param name_or_path The name of the function or filesystem path to it's source code
#' @param func A function. Optional.
#' @param xml A XML specification of the function. Optional.
#'
#' @export
function_register <- function(name_or_path, func, xml) {
  rd <- NULL
  if (file.exists(name_or_path)) {
    path <- name_or_path
    # Source the file into an environment and 'extract' the function
    env <- new.env()
    source(path, local = env)
    names <- ls(env)
    if (length(names) > 1) stop("More than one name defined in source file: \"", path, "\" has \"", paste(names, collapse = ","), "\"")
    name <- names[1]
    if (paste0(name, ".R") != basename(path)) stop("Name of function is not consistent with name of file: \"", paste0(name, ".R"), "\" != \"", basename(path), "\"")
    func <- get(name, envir = env)
    # Get documentation block from file if available
    blocks <- roxygen2::parse_file(path)
    if (length(blocks) > 0) {
      # Convert to Rd
      rd_text <- roxygen2:::block_to_rd(blocks[[1]], ".")$format()
      # Parse the generated .Rd file
      rd <- tools::parse_Rd(textConnection(rd_text))
    }
  } else {
    name <- name_or_path
  }

  if (missing(func)) {
    rd_name <- name
    func <- get(name)
  } else if (mode(func) == "character") {
    rd_name <- func
    func <- get(func)
  }

  if (missing(xml)){
    docs <- NULL

    if (is.null(rd)) {
      # Get the Rd based documentation, if any, for the function
      rd_files <- utils::help(rd_name)
      if (length(rd_files) > 0) {
        # Currently, taking the first found file and convert it into a list
        rd <- utils:::.getHelpFile(rd_files[1])
      }
    }

    if (!is.null(rd)) {
      #Convert Rd to docs list
      # Thanks to Jeroen at http://stackoverflow.com/questions/8918753/r-help-page-as-object
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
      # Extract docs from the functions source code
      src <- deparse(func, width.cutoff = 500, nlines = 1)
      pars <- str_match(src, "function \\((.*)\\)")[1, 2]
      pars <- str_split(pars, ",")[[1]]
      pars <- str_match(pars, "(\\w+)( = (.+))?")
      # Convert each parameter into a list of the form expected
      # to be in the `arguments` member of the documentation
      # Currently the default value for a parameter is parsed out
      # but not used here.
      args <- list()
      for (row in 1:nrow(pars)) {
        args[[row]] <- list(
          arg = pars[row, 2],
          description = ""
        )
      }
      docs <- list(arguments = args)
    }

    # Override the name in the docs to the name of this function.
    # This is necessary when `help_name` is different from `name`
    # e.g. `cor_test` v `cor.test`
    docs[["name"]] <- name

    # Extract properties from the docs into a specification
    spec <- xml_new_root("function")
    add <- function(from, to) {
      if (!is.null(docs[[from]])) xml_add_child(spec, to, docs[[from]])
    }
    add("name", "name")
    add("description", "summary")
    add("details", "description")

    params <- xml_add_child(spec, "params")
    lapply(docs$arguments, function(arg) {
      param <- xml_add_child(params, "param", name = arg$arg)
      xml_add_child(param, "description", arg$description)
    })

    xml_add_child(spec, "return", docs[["value"]])

    implems <- xml_add_child(spec, "implems")
    xml_add_child(implems, "implem", language = "r")

    # Rd examples area single single of code so put it
    # into <examples><example><usage>
    if (!is.null(docs[["examples"]])) {
      examples <- xml_add_child(spec, "examples")
      example <- xml_add_child(examples, "example")
      xml_add_child(example, "usage", docs[["examples"]])
    }

    # Jump through hoops to get XML string...
    raw <- xml_serialize(spec, NULL)
    raw <- raw[raw != 0]
    char <- rawToChar(raw)
    xml <- str_sub(char, str_locate(char, "<function>")[1], str_locate(char, "</function>")[2])
  }

  assign(name, func, envir = functions)
  assign(name, xml, envir = functions_xml)

  list(name = name, func = func, xml = xml)
}

function_test <- function(path) {
}

function_document <- function(xml, dest) {
  doc <- xml2::read_xml(xml)
  md <- file(dest, "w")
  cat("#", xml2::xml_text(xml2::xml_find_first(doc, ".//name")), file = md)
}

function_list <- function() {
  ls(functions)
}

#'' Register a library of functions
#''
#' Registers the function in each `.R` file in a library directory.
#'
#' @param path File system path to the library
#'
#' @export
library_register <- function(path) {
  if (!dir.exists(path)) stop("Path does not exist \"", path, "\"")
  files <- Sys.glob(file.path(path, "funcs", "*.R"))
  if (length(files) == 0) stop("No functions found in \"", file.path(path, "funcs"), "\"")
  for (file in files) function_register(file)
}

#' @export
library_test <- function(path = ".") {
  # Get all functions
  funcs_dir <- file.path(path, "funcs")
  files <- Sys.glob(file.path(funcs_dir, "*.R"))
  if (!length(files)) stop("No R files in \"", funcs_dir, "\"")
  # For each file run it's tests
  for (file in files) {
    # Register the function
    registered <- function_register(file)
    # Check that there is a test file for the function
    test <- file.path(path, "tests", paste0(registered$name, ".R"))
    if (!file.exists(test)) stop("No test file for function \"", registered$name, "\"")
    # Attach the function to the search path
    env <- list()
    env[[registered$name]] <- registered$func
    attach(env)
    # Run the test file
    testthat::test_file(test)
  }
}

#' Register a library of functions
#'
#' Registers the function in each `.R` file in a library directory.
#'
#' @param path File system path to the library
#'
#' @export
library_document <- function(path = ".") {
  # Get all functions
  funcs_dir <- file.path(path, "funcs")
  files <- Sys.glob(file.path(funcs_dir, "*.R"))
  if (!length(files)) stop("No R files in \"", funcs_dir, "\"")
  # Generate documentation for each function
  for (file in files) {
    # Register the function
    registered <- function_register(file)
    # Generate Markdown documentation for static site
    function_document(registered$xml, file.path(path, "docs", paste0(registered$name, ".md")))
  }
}
