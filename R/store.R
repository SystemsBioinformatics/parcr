#' The `.parcr` environment is used for storing and retrieving variables by the
#' user.
#' @returns Nothing
#' @noRd
.parcr <- list2env(list(), parent = emptyenv())

#' Store and retrieve objects
#'
#' Sometimes you want to use a parsed object to modify a later parser operation,
#' as in the example below. The `store()` and `retrieve()` functions provide the
#' tools to create such a parser.
#'
#' @param name a string used as the name of the stored object.
#' @param value object to be stored.
#'
#' @returns Nothing for `store()` and the stored object for `retrieve()`.
#' @export
#'
#' @examples
#' parse_nr <- function(line) {
#'   m <- stringr::str_extract(line, "number=(\\d+)", group = 1)
#'   if (is.na(m)) {
#'     list()
#'   } else {
#'     store("nr", as.numeric(m))
#'   }
#' }
#'
#' p <- function() {
#'   match_s(parse_nr) %then%
#'     exactly(retrieve("nr"), literal("A"))
#' }
#'
#' p()(c("number=3", "A", "A", "A")) # success
#' p()(c("number=2", "A", "A", "A")) # failure
store <- function(name, value) {
  .parcr[[name]] <- value
}

#' Retrieve an object
#'
#' @export
#' @rdname store
#'
retrieve <- function(name) {
  if (!exists(name, envir = .parcr)) {
    stop("Variable '", name, "' not found in store. Use store() to set it first.")
  }
  .parcr[[name]]
}

#' Clear all stored values
#'
#' Removes all variables stored using \code{store()}. Use this to clean up
#' between independent parser runs or in test teardown.
#'
#' @export
clear_store <- function() {
  rm(list = ls(.parcr), envir = .parcr)
}

#' List all stored variable names
#'
#' Returns the names of all variables currently stored in the parser environment.
#'
#' @return Character vector of variable names
#' @export
list_stored <- function() {
  ls(.parcr)
}

#' Check if a variable is stored
#'
#' @param name Variable name to check
#' @return Logical
#' @export
has_stored <- function(name) {
  exists(name, envir = .parcr)
}
