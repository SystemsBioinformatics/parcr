# More or less following Graham Hutton's paper
# https://doi.org/10.1017/S0956796800000411

## Primitive parsers
# The primitive parsers are the building blocks of combinator parsing.

#' The most basic parsers.
#'
#' @description
#'
#' These are the most basic constructors of a parser, but they are important
#' cogs of the parser machinery. The `succeed` parser always succeeds, without
#' consuming any input, whereas the `fail` parser always fails.
#'
#' @details
#' The `succeed` parser constructs a `list` object with a 'left' or L-part
#' that contains the parser result of the consumed part of the input vector and
#' the 'right' or R-part that contains the unconsumed part of the vector. Since
#' the outcome of succeed does not depend on its input, its result value must
#' be pre-determined, so it is included as an extra parameter.
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
#' You will probably never have to use these functions when constructing
#' parsers.
#'
#' @param left any R-object constructed from a parsed vector.
#' @export
#' @examples
#' succeed("A")("abc")
#' succeed(data.frame(title="Keisri hull", author="Jaan Kross"))(c("Unconsumed","text"))
succeed <- function(left) {
  function(right) list(L=ensure.list(left), R=right)
}

# The parser that always fails.
#' @rdname succeed
#' @export
#' @examples
#' fail()("abc")
#'
fail <- function() {
  function(x) list()
}

#' The parser that matches an element using a predicate function.
#'
#' @description
#' `satisfy` turns a predicate function into a parser that recognizes strings.
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
#' @export
#' @examples
#'
#' # define a predicate function that tests whether the next element starts
#' # with an 'a'
#' starts_with_a <- function(x) grepl("^a",x)
#' # Use it in the satisfy parser
#' satisfy(starts_with_a)(c("abc","def")) # success
#' satisfy(starts_with_a)(c("bca","def")) # failure
#' # Using an anonymous function
#' satisfy(function(x) {as.numeric(x)>10})("15") # success
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

#' The parser that matches an element using a literal string.
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
#' @param string a single element character vector.
#' @export
#' @examples
#' literal("ab") (c("ab", "cdef")) # success
#' literal("ab") (c("abc", "cdef")) # failure
literal <- function(string) {
  satisfy(function(x) identical(x, as.character(string)))
}

#' Detect end of input.
#'
#' @description
#' `eof` tests whether the end of the input character vector has been reached,
#' which boils down to detection of `character(0)` as input. Since the intended
#' application of this parser is parsing of text files the function has been
#' called after the end of file (EOF) signal. To indicate that an end of file
#' has been detected, the right hand side of the parser result will be
#' converted to an empty list.
#'
#' @section Pseudocode:
#' \preformatted{
#' eof()(x):
#'   if x==null then succeed(x)(list())
#'   else fail()(x)
#' }
#'
#' @return A parser
#' @export
#'
#' @examples
#' (literal("a") %then% eof())("a") # success
#' # Notice the difference on the R-side with
#' literal("a")("a")
#' eof()(character(0)) # success
#' eof()("a") # failure
eof <- function() {
  function(x) {
    if (is_empty_atom(x)) succeed(x)(list())
    else fail()(x)
  }
}

## Combinators
# Above we made the basic building blocks. We consider how they should be put
# together to form useful parsers.

#' The parser of alternative parsers.
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
#' @returns A parser.
#' @export
#' @examples
#' (literal("A") %or% literal("a"))(LETTERS[1:5]) # success on first parser
#' (literal("A") %or% literal("a"))(letters[1:5]) # success on second parser
#' (literal("A") %or% literal("a"))(LETTERS[2:6]) # failure
#' starts_with_a <- function(x) grepl("^a",x[1])
#' # success on both parsers, but returns result of p1 only
#' (literal("a") %or% satisfy(starts_with_a)) (letters[1:5])
`%or%` <- function(p1, p2) {
  function(x) {
    r1 <- p1(x)
    r2 <- p2(x)
    if (!failed(r1)) r1 else {if (!failed(r2)) r2 else fail()(x)}
  }
}

#' The parser of sequences of parsers.
#'
#' @description
#'
#' `(p1 %then% p2)` recognizes anything that `p1` and `p2` would if placed in
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
#' @returns A parser.
#' @export
#' @seealso The discarding versions [%xthen%] and [%thenx%]
#' @examples
#' starts_with_a <- function(x) grepl("^a",x[1])
#' starts_with_b <- function(x) grepl("^b",x[1])
#' (satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("ab", "bc", "de")) # success
#' (satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("bb", "bc", "de")) # failure
#' (satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("ab", "ac", "de")) # failure
`%then%` <- function(p1, p2) {
  function(x) {
    # Fail on NULL input, otherwise we create endless loops
    if (is_empty_atom(x)) fail()(x)
    else {
      r1 <- p1(x)
      if (failed(r1)) fail()(x)
      else {
        r2 <- p2(r1$R)
        if (failed(r2)) fail()(x) else succeed(c(r1$L, r2$L)) (r2$R)
      }
    }
  }
}

#' Manipulate results from a parser by applying a function.
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
#' @param f a function to be applied to the result of a succesful `p`.
#' @returns A parser.
#' @export
#' @examples
#' (literal('ab') %using% toupper) (c("ab","cdef")) # success
#' (literal('ab') %using% toupper) (c("bb","cdef")) # failure
`%using%` <- function(p, f) {
  function(x) {
    r <- p(x)
    if (failed(r)) fail()(x) else succeed(f(r$L))(r$R)
  }
}

#' Keeping only the left or right result from a `%then%` sequence.
#'
#' @details
#' Recall that two parsers composed in sequence produce a pair of results.
#' Sometimes we are only interested in one component of the pair. For example,
#' it is common to throw away reserved words such as 'begin' and 'where' during
#' parsing. In such cases, two special versions of the `%then%` combinator are
#' useful, which throw away either the left or right result values, as reflected
#' by the position of the letter 'x' in their names.
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
#'
#' @return A parser
#' @export
#'
#' @examples
#' is_number <- function(x) grepl("\\d+",x[1])
#' # Numbers are preceded by ">" symbols, but we only need the number itself
#' (literal(">") %thenx% satisfy(is_number)) (c(">", "12"))
#' # Temperatures are followed by the unit 'C', but we only need the number
#' (satisfy(is_number) %xthen% literal("C")) (c("21", "C"))
`%xthen%` <- function(p1, p2) {
  function(x) {
    # Fail on NULL input, otherwise we create endless loops
    if (is_empty_atom(x)) fail()(x)
    else {
      r1 <- p1(x)
      if (failed(r1)) fail()(x)
      else {
        r2 <- p2(r1$R)
        if (failed(r2)) fail()(x) else succeed(r1$L) (r2$R)
      }
    }
  }
}

#' @rdname grapes-xthen-grapes
#' @export
#' @seealso [%then%]
`%thenx%` <- function(p1, p2) {
  function(x) {
    # Fail on NULL input, otherwise we create endless loops
    if (is_empty_atom(x)) fail()(x)
    else {
      r1 <- p1(x)
      if (failed(r1)) fail()(x)
      else {
        r2 <- p2(r1$R)
        if (failed(r2)) fail()(x) else succeed(r2$L) (r2$R)
      }
    }
  }
}

#' Return a fixed value upon successful parsing.
#'
#' @description
#' Sometimes we are not interested in the result from a parser at all, only
#' that the parser succeeds. For example, if we find a reserved word during
#' lexical analysis, it may be convenient to return some short representation
#' rather than the string itself. The `%ret%` combinator is useful in such
#' cases. The parser `(p %ret% c)` has the same behavior as `p`, except that it
#' returns the value `c` if successful
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
#' @param c a single-element character value. `NULL` is coerced to
#'  `character()`.
#'
#' @returns A parser
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

## Repeated application of parsers.

#' Parsers that repeat application of a parser.
#'
#' @description
#' Often, you expect repetition of a structure that can be parsed by a parser
#' `p`, or you expect that a parser zero or more times. The following parsers
#' these conditions.
#'
#' @details
#' All these parsers except `match_n` are greedy. This means that they try to
#' apply `p` as many times as possible. If that number of times is not equal to
#' what was expected, for example if `p` can successfully parse more than `n`
#' times in `exactly(n,p)`, then the parser will fail. In contrast,
#' `match_n(n,p)` will apply `p` `n` times and no more, even if `p` could be
#' successfully applied more often. Clearly, both functions will fail if `p`
#' leads to failure after less than `n` repetitions.
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
#'   r = zero_or_more(p)(x)
#'   if length(r[1]) == n) then r else fail()(x)
#'
#' zero_or_one:
#'   exactly(1,p) \%or\% exactly(0,p)
#'
#' match_n(n,p):
#'   if n==1 then p else (p \%then\% match_n(n-1, p))
#' }
#'
#' where `null` is the empty vector.
#'
#' @returns A parser
#' @export
#' @examples
#' zero_or_more(literal("A")) (c("A",LETTERS[1:5]))
#' zero_or_more(literal("A")) (LETTERS[2:5])
zero_or_more <- function(p) {
  (p %then% zero_or_more(p)) %or% succeed(character())
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
  function(x) {
    r <- zero_or_more(p)(x)
    if (length(r$L) == n) r else fail()(x)
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
  if (n == 1) p else (p %then% match_n(n - 1, p))
}

#' The parser that identifies a string and produces custom output.
#'
#' @description
#' `match_s` matches a string using a function and returns a desired object type.
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
#' signal failure to the parser it must return `list()` as the output.
#'
#' @section Pseudocode:
#' \preformatted{
#' match_s(s)(x):
#'   if x==null then fail()(x)
#'   else if s(x[1]) then succeed(s(x[1]))(x[-1]) else fail()(x)
#' }
#'
#' @param s A string-parsing function.
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
