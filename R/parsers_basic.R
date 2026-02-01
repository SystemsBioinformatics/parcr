# More or less following Graham Hutton's paper
# https://doi.org/10.1017/S0956796800000411

## Primitive parsers
# The primitive parsers are the building blocks of combinator parsing.

#' The most basic parsers
#'
#' @description
#'
#' These are the most basic constructors of a parser, but they are important
#' cogs of the parser machinery. The `succeed` parser always succeeds, without
#' consuming any input, whereas the `fail` parser always fails.
#'
#' @details
#' The `succeed` parser constructs a `list` object with a 'left' or `L`-element
#' that contains the parser result of the consumed part of the input vector and
#' the 'right' or `R`-element that contains the unconsumed part of the vector.
#' Since the outcome of succeed does not depend on its input, its result value
#' must be pre-determined, so it is included as a parameter.
#'
#' While `succeed` never fails, `fail` always does, regardless of the input
#' vector. It returns the empty list `list()` to signal this fact.
#'
#' @section Pseudocode:
#' \preformatted{
#' succeed(y)(x): [L=[y],R=[x]]
#' fail()(x):     []
#' }
#'
#' where `[L=[y],R=[x]]` is a named list with lists `[y]` and `[x]` as elements
#' and `[]` is an empty list.
#'
#' @section Note:
#' It is very unlikely that you will ever have to use these functions when
#' constructing parsers.
#'
#' @param left any R-object constructed from a parsed vector.
#' @returns A list. `succeed()` returns a list with two elements named `L` and
#'  `R`. `fail()` returns a `marker` object which is basically an empty list
#'  with a line number `n` as attribute. It is printed as the icon `[]`,
#'  see [print.marker()]. Note that `n` will only correctly represent the line
#'  number of failure when a parser is wrapped in the [reporter()] function.
#'
#' @export
#' @examples
#' succeed("A")("abc")
#' succeed(data.frame(title="Keisri hull", author="Jaan Kross"))(c("Unconsumed","text"))
#'
succeed <- function(left) {
  function(right) list(L=ensure.list(left), R=right)
}

# The parser that always fails
#' @rdname succeed
#'
#' @param lnr integer. The line number (element number) at which the fail
#'            occurs
#' @export
#' @examples
#' fail()("abc")
#'
fail <- function(lnr=LNR()) {
  function(x) new_marker(lnr)
}

#' Matching input using a logical function
#'
#' @description
#' `satisfy()` turns a logical function into a parser that recognizes strings.
#'
#' @details
#' Notice (see pseudocode) that `satisfy` fails when presented with empty
#' input, so it is futile to write predicate functions that would recognize
#' such input.
#'
#' @section Pseudocode:
#' \preformatted{
#' satisfy(b)(x):
#'   if x==null then fail()(x)
#'   else if b(x[1]) then succeed(x[1])(x[-1]) else fail()(x)
#' }
#'
#' where `x[1]` is the first element of `x`, `x[-1]` all subsequent elements
#' (or `null` if it only has one element). `null` is the empty vector,
#' equivalent to `character(0)` in R.
#'
#' @param b a boolean function to determine if the string is accepted.
#' @returns A parser.
#' @export
#' @examples
#' # define a predicate function that tests whether the next element starts
#' # with an 'a'
#' starts_with_a <- function(x) grepl("^a",x)
#' # Use it in the satisfy parser
#' satisfy(starts_with_a)(c("abc","def")) # success
#' satisfy(starts_with_a)(c("bca","def")) # failure
#' # Using an anonymous function
#' satisfy(function(x) {as.numeric(x)>10})("15") # success
#'
satisfy <- function(b) {
  function(x) {
    if (is_empty_atom(x)) fail()(x)
    else {
      l <- x[1]
      r <- x[-1]
      if (b(l)) succeed(l)(r) else fail()(x)
    }
  }
}

#' Matching parser input with a literal string
#'
#' @description
#' `literal` tests whether a supplied string literally equals a desired value.
#'
#' @section Pseudocode:
#' \preformatted{
#' literal(a)(x): satisfy(F(y): y==a)(x)
#' }
#'
#' where `F` is equivalent to the `function` declarator in R. So, we have an
#' anonymous function in the argument of `satisfy`.
#'
#' @param string string, a single-element character vector, or an object that
#'   can be coerced to a character vector.
#' @inherit satisfy return
#' @export
#' @examples
#' literal("ab") (c("ab", "cdef")) # success
#' literal("ab") (c("abc", "cdef")) # failure
#'
literal <- function(string) {
  satisfy(function(x) identical(x, as.character(string)))
}

#' Detect end of input
#'
#' @description
#' Tests whether the end of the input character vector has been reached,
#' which boils down to detection of `character(0)` in the `R`-element (see
#' [succeed()]). Since the intended application of this parser is parsing of
#' text files the function has been called after the end of file (EOF) signal.
#' To indicate that an end of file has been detected, the `R`-element side of
#' the parser output will be converted to an empty list.
#'
#' @section Pseudocode:
#' \preformatted{
#' eof()(x):
#'   if x==null then succeed(x)(list())
#'   else fail()(x)
#' }
#'
#' @inherit satisfy return
#' @export
#'
#' @examples
#' (literal("a") %then% eof())("a") # success
#' # Notice the difference on the R-side with
#' literal("a")("a")
#' eof()(character(0)) # success
#' eof()("a") # failure
#'
eof <- function() {
  function(x) {
    if (is_empty_atom(x)) succeed(x)(list())
    else fail()(x)
  }
}

## Combinators
# Above we made the basic building blocks. We consider how they should be put
# together to form useful parsers.

#' Applying alternative parsers
#'
#' @description
#' The `%or%` combinator `(p1 %or% p2)` returns the result of `p1` if `p1` is
#' successful or, if `p1` fails that of `p2` if `p2` parses successfully,
#' otherwise it returns a `fail`.
#'
#' @section Pseudocode:
#' \preformatted{
#' (p1 \%or\% p2)(x):
#'   if p1(x)==[] then
#'     if p2(x)==[] then fail()(x) else p2(x)
#'   else p1(x)
#' }
#'
#' where `[]` is the empty list.
#'
#' @param p1,p2 two parsers.
#' @inherit satisfy return
#' @export
#' @examples
#' (literal("A") %or% literal("a"))(LETTERS[1:5]) # success on first parser
#' (literal("A") %or% literal("a"))(letters[1:5]) # success on second parser
#' (literal("A") %or% literal("a"))(LETTERS[2:6]) # failure
#' starts_with_a <- function(x) grepl("^a",x[1])
#' # success on both parsers, but returns result of p1 only
#' (literal("a") %or% satisfy(starts_with_a)) (letters[1:5])
#'
`%or%` <- function(p1, p2) {
  function(x) {
    init_lnr <- LNR()
    r1 <- p1(x)
    if (!failed(r1)) r1 else {
      set_LNR(init_lnr) # reset to where started
      r2 <- p2(x)
      if (!failed(r2)) r2 else {
        line_nrs <- c(marker_val(r1), marker_val(r2))
        furthest <- max(line_nrs)
        return(fail(lnr = furthest)(x))
      }
    }
  }
}

#' Applying parsers in sequence
#'
#' @description
#'
#' `(p1 %then% p2)` recognizes anything that `p1` and `p2` would if applied in
#' succession.
#'
#' @section Pseudocode:
#' \preformatted{
#' (p1 \%then\% p2)(x):
#'   if p1(x)==[] or x==null then fail()(x)
#'   else
#'     if p2(x[-1])==[] then fail()(x)
#'     else succeed([p1(x)$L, p2(x[-1])$L])(x[-2])
#' }
#'
#' where `null` is the empty vector, `x[-1]` and `x[-2]` are the vector `x`
#' without the first element and without the first two elements, respectively.
#'
#' @inheritParams %or%
#' @inherit satisfy return
#' @export
#' @seealso The discarding versions [%xthen%] and [%thenx%]
#' @examples
#' starts_with_a <- function(x) grepl("^a",x[1])
#' starts_with_b <- function(x) grepl("^b",x[1])
#' (satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("ab", "bc", "de")) # success
#' (satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("bb", "bc", "de")) # failure
#' (satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("ab", "ac", "de")) # failure
#'
`%then%` <- function(p1, p2) {
  function(x) {
    r1 <- p1(x)
    if (failed(r1)) r1
    else {
      inc_LNR()
      r2 <- p2(r1$R)
      if (failed(r2)) r2 else succeed(c(r1$L, r2$L))(r2$R)
    }
  }
}

#' Applying a function to the result of a parser
#'
#' @description
#' The `%using%` combinator allows us to manipulate results from a parser. The
#' parser `(p %using% f)` has the same behavior as the parser `p`, except that
#' the function `f` is applied to its result value.
#'
#' @section Pseudocode:
#'
#' \preformatted{
#' (p \%using\% f)(x):
#'   if p1(x)==[] then fail()(x)
#'   else succeed(f(p1(x)$L))(x[-1])
#' }
#'
#' @inheritParams zero_or_more
#' @param f a function to be applied to the result of a successful `p`.
#' @inherit satisfy return
#' @export
#' @examples
#' (literal('ab') %using% toupper) (c("ab","cdef")) # success
#' (literal('ab') %using% toupper) (c("bb","cdef")) # failure
#'
`%using%` <- function(p, f) {
  function(x) {
    r <- p(x)
    if (failed(r)) fail()(x) else succeed(f(r$L))(r$R)
  }
}

#' Keeping only first or second result from a `%then%` sequence
#'
#' @description
#' Two parsers composed in sequence produce a pair of results. Sometimes we are
#' only interested in one component of the pair. For example in the case of
#' reserved words such as 'begin' and 'end'. In such cases, two special
#' versions of the `%then%` combinator are useful, which keep either the
#' first or second result, as reflected by the position of the letter 'x' in
#' their names.
#'
#' @section Pseudocode:
#'
#' \preformatted{
#' (p1 \%xthen\% p2)(x):
#'   if p1(x)==[] or x==null then fail()(x)
#'   else
#'     if p2(x[-1])==[] then fail()(x)
#'     else succeed(p1(x)$L)(x[-2])
#'
#' (p1 \%thenx\% p2)(x):
#'   if p1(x)==[] or x==null then fail()(x)
#'   else
#'     if p2(x[-1])==[] then fail()(x)
#'     else succeed(p2(x[-1])$L)(x[-2])
#' }
#' where `null` is the empty vector, `x[-1]` and `x[-2]` are the vector `x`
#' without the first element and without the first two elements, respectively.
#'
#' @inheritParams %or%
#' @inherit satisfy return
#' @export
#'
#' @examples
#' is_number <- function(x) grepl("\\d+",x[1])
#' # Numbers are preceded by ">" symbols, but we only want the number
#' (literal(">") %thenx% satisfy(is_number)) (c(">", "12"))
#' # Temperatures are followed by the unit 'C', but we only want the number
#' (satisfy(is_number) %xthen% literal("C")) (c("21", "C"))
#'
`%xthen%` <- function(p1, p2) {
  function(x) {
    r1 <- p1(x)
    if (failed(r1)) r1
    else {
      inc_LNR()
      r2 <- p2(r1$R)
      if (failed(r2)) r2 else succeed(r1$L) (r2$R)
    }
  }
}

#' @rdname grapes-xthen-grapes
#' @export
#' @seealso [%then%]
#'
`%thenx%` <- function(p1, p2) {
  function(x) {
    r1 <- p1(x)
    if (failed(r1)) r1
    else {
      inc_LNR()
      r2 <- p2(r1$R)
      if (failed(r2)) r2 else succeed(r2$L) (r2$R)
    }
  }
}

#' Return a fixed value instead of the result of a parser
#'
#' @description
#' Sometimes we are not interested in the result from a parser, only that the
#' parser succeeds. It may be convenient to return some short representation
#' or nothing even rather than the string itself. The `%ret%` combinator is
#' useful in such cases. The parser `(p %ret% c)` has the same behavior as `p`,
#' except that it returns the value `c` if successful.
#'
#' @section Pseudocode:
#'
#' \preformatted{
#' (p \%xret\% c)(x):
#'   if p(x)==[] then fail()(x)
#'   else succeed(c)(x[-1])
#' }
#'
#' @inheritParams zero_or_more
#' @param c string, _i.e._ a single-element character vector. `NULL` is coerced
#'  to `character(0)`.
#'
#' @inherit satisfy return
#' @export
#'
#' @seealso [%using%]
#' @examples
#' (literal("A") %ret% "We have an A!") (LETTERS[1:5])
#' (literal("A") %ret% NULL) (LETTERS[1:5])
`%ret%` <- function(p, c) {
  function(x) {
    r <- p(x)
    if (failed(r)) fail()(x) else succeed(as.character(c))(r$R)
  }
}

## Repeaters

#' Repeated application of a parser
#'
#' @description
#' Often, we want to assess whether a given structure can be successfully
#' parsed through repetitive application of a parser `p`. This could involve
#' testing the parser applied multiple times in succession or determining
#' its capability to be applied zero or more times.
#'
#' The subsequent functions are designed to address and evaluate these
#' scenarios.
#'
#' @details
#' All these parsers with the exception of `match_n` exhibit greedy behavior
#' striving to apply `p` as many times as possible. If the resulting count
#' doesn't match the expected quantity, such as in the case of `exactly(n,p)`
#' where `p` successfully parses more than `n` times, then the parser fails.
#' In contrast, `match_n(n,p)` strictly applies `p` exactly `n` times,
#' preventing any further application of `p` even if `p` could potentially be
#' applied more often. Clearly, both functions will fail if `p` fails after
#' less than `n` repetitions.
#'
#'
#'
#' @param p a parser.
#'
#' @section Pseudocode:
#' \preformatted{
#' zero_or_more(p):
#'   (p \%then\% zero_or_more(p)) \%or\% succeed(null)
#'
#' one_or_more(p):
#'   p \%then\% zero_or_more(p)
#'
#' exactly(n,p):
#'   count = 0
#'   r = zero_or_more(p \%using\% F(x): count = count + 1; x)(x)
#'   if count == n then
#'     count = 0
#'     r
#'   else fail()(x)
#'
#' zero_or_one:
#'   exactly(1,p) \%or\% exactly(0,p)
#'
#' match_n(n,p):
#'   if n==0 then F(x): succeed(list())(x)
#'   else
#'     if n==1 then p else (p \%then\% match_n(n-1, p))
#' }
#'
#' where `null` is the empty vector.
#'
#' @inherit satisfy return
#' @export
#' @examples
#' zero_or_more(literal("A")) (c("A",LETTERS[1:5]))
#' zero_or_more(literal("A")) (LETTERS[2:5])
#'
zero_or_more <- function(p) {
  (p %then% zero_or_more(p)) %or% (succeed(character(0)) %using% function(x){dec_LNR(); x})
}

#' @rdname zero_or_more
#' @export
#' @examples
#' one_or_more(literal("A")) (c("A",LETTERS[1:5])) # success
#' one_or_more(literal("A")) (LETTERS[2:5]) # failure
#'
one_or_more <- function(p) {
  p %then% zero_or_more(p)
}

#' @rdname zero_or_more
#' @param n a positive integer, including 0.
#' @export
#' @examples
#' exactly(2,literal("A")) (c("A", LETTERS[1:5])) # success
#' exactly(2,literal("A")) (c(rep("A",2), LETTERS[1:5])) # failure: too many "A"
#'
exactly <- function(n, p) {
  # notice that this is a greedy parser due to zero_or_more's greediness
  # The non-greedy version is match_n
  stopifnot(n >= 0)
  stopifnot(as.integer(n) == n)
  cnt <- 0
  function(x) {
    r <- zero_or_more((p) %using% function(x) {cnt <<- cnt + 1; return(x)})(x)
    if (cnt == n) {
      # reset cnt
      cnt <<- 0
      return(r)
    }
    else {
      # cnt <<- 0
      return(fail()(x))
    }
  }
}

#' @rdname zero_or_more
#' @export
#' @examples
#' zero_or_one(literal("A")) (LETTERS[2:5]) # success
#' zero_or_one(literal("A")) (LETTERS[1:5]) # success
#' zero_or_one(literal("A")) (c("A",LETTERS[1:5])) # failure
#'
zero_or_one <- function(p) {
  exactly(1,p) %or% exactly(0,p)
}

#' @rdname zero_or_more
#' @export
#' @examples
#' match_n(2,literal("A")) (c("A", LETTERS[1:5])) # success
#' match_n(2,literal("A")) (c(rep("A",2), LETTERS[1:5])) # success
#'
match_n <- function(n, p) {
  # non-greedy version of 'exactly'
  stopifnot(n >= 0)
  stopifnot(as.integer(n) == n)
  if (n == 0) function(x) {succeed(list())(x)}
  else {
    if (n == 1) p else (p %then% match_n(n - 1, p))
  }
}

#' Identifying and processing a string and producing custom output
#'
#' `match_s` matches a string using a function and returns a desired object
#' type.
#'
#' @details
#' This parser short-cuts the pattern `satisfy(b) %using% f`. With `match_s`
#' you do not have to write separate predicate and processing functions `b` and
#' `f` when identification and parsing can be done with a single string
#' parsing function `s`.
#'
#' The function `s` will be given a non-empty single-element character vector
#' as its argument, so you don't have to test for empty input, like
#' `character(0)`. These two facts also often simplify further processing with
#' the string functions like `grep`, `regmatches` and those from the `stringr`
#' package. The function `s` can return any R-object when succeeding, but to
#' signal failure to the parser it must return the empty `list()`. Note that
#' `list()` output from `s` will be turned into a marker object, the internal
#' object to mark failure, by `match_s()`, see [failed()].
#'
#' @section Pseudocode:
#' \preformatted{
#' match_s(s)(x):
#'   if x==null then fail()(x)
#'   else if s(x[1]) then succeed(s(x[1]))(x[-1]) else fail()(x)
#' }
#'
#' @param s a string-parsing function.
#' @inherit satisfy return
#' @export
#' @examples
#' expect_integers <- function(x) {
#'   m <- gregexpr("[[:digit:]]+", x)
#'   matches <- regmatches(x,m)[[1]]
#'   if (length(matches)==0) {
#'     # this means failure to detect numbers where we expected them
#'     return(list())
#'   } else {
#'     return(as.numeric(matches))
#'   }
#' }
#'
#' match_s(expect_integers) ("12 15 16 # some comment") # success
#' match_s(expect_integers) ("some text") # failure
#'
match_s <- function(s) {
  function(x) {
    if (is_empty_atom(x)) fail()(x)
    else {
      l <- s(x[1])
      r <- x[-1]
      if (failed(l)) fail()(x) else succeed(l)(r)
    }
  }
}
