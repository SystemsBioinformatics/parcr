#' The `.parcr` environment is used for storing and retrieving variables by the
#' user.
#' @returns \value{None}
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
#' @returns \value{None} for `store()` and the stored object for `retrieve()`.
#' @export
#'
#' @examples
#' parse_nr <- function(line) {
#'   m <- stringr::str_extract(line, "number=(\\d+)", group=1)
#'   if (is.na(m)) list()
#'   else store("nr", as.numeric(m))
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
  .parcr[[name]]
}
