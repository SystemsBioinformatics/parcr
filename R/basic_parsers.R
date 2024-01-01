# More or less following Graham Hutton's paper
# https://doi.org/10.1017/S0956796800000411

## Primitive parsers
# The primitive parsers are the building blocks of combinator parsing.

#' The beginning and the end.
#'
#' @description
#'
#' These are the most banal constructors of a parser, but they are important
#' cogs in the machinery. The `succeed` parser always succeeds, without
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
#' @section Note:
#' You will probably never have to use these functions when constructing parsers.
#'
#' @param left Any R-object constructed from a parsed vector.
#' @export
#' @examples
#' succeed("A")("abc")
#' succeed(data.frame(title="Keisri hull", author="Jaan Kross"))(c("Unconsumed","text"))
succeed <- function(left) {
  function(right) list(L=ensure.list(left), R=right)
}

# The parser that always fails
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
#' `satisfy` turns a predicate function into a parser that recognizes single
#' elements.
#'
#' @section Definition:
#'
#' ```
#' satisfy(b)(x): fail()(x)             when x = NULL
#'              : succeed(x[1]) (x[-1]) when b(x[1]])
#'              : fail()(x)             otherwise
#' ```
#'
#' @param b A boolean function to determine if the element is accepted.
#' @export
#' @examples
#'
#' # define a predicate function that tests whether the next element starts
#' # with an 'a'
#' starts_with_a <- function(x) grepl("^a",x)
#' # Use it in the satisfy parser
#' satisfy(starts_with_a)(c("abc","def")) # success
#' satisfy(starts_with_a)(c("bca","def")) # failure
satisfy <- function(b) {
  function(x) {
    if (is.empty(x)) {
      l <- x
      r <- x
    } else {
      l <- x[1]
      r <- x[-1]
    }
    if (b(l)) succeed(l)(r) else fail()(cv)
  }
}

#' The parser that matches an element using a literal string.
#'
#' @description
#' `literal` is a parser for single elements. It tests whether the first
#' element in the vector is equal to a given element.
#'
#' @section Definition:
#'
#' ```
#' literal(a)(x) <- satisfy(function(y) {identical(y,a)}) (x)
#' ```
#'
#' where `= x` is to be understood as a function which tests its argument for
#' equality with `x`
#'
#' @param element A single-element (character) vector to be matched with the
#' first element of the right hand side
#' @export
#' @examples
#' literal("ab") (c("ab", "cdef")) # success
#' literal("ab") (c("abc", "cdef")) # failure
literal <- function(element) {
  satisfy(
    function(x) {
      if (is.empty(x)) {first.element <- x} else {first.element <- x[1]}
      return(identical(first.element, element))
      #return(first.element==element)
    }
  )
}

## Combinators
# Now that we have the basic building blocks, we consider how they should be put
# together to form useful parsers.

#' The parser of alternative parsers.
#'
#' @description
#' The `%or%` combinator `(p1 %or% p2)` returns the result of `p1` if `p1` is
#' successful or, if `p1` fails that of `p2` if `p2` parses successfully,
#' otherwise it returns a `fail`.
#'
#' @param p1,p2 Two parsers
#' @returns A list of parser results
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
#' @section Definition:
#'
#'  ```
#' (p1 %then% p2) (x) = [((v1,v2),out2) | (v1,outl) <- p1 inp;`
#'                                       (v2,out2) <- p2 out1]`
#' ```
#' ```
#' (p1 %then% p2) (x): fail()(x)             when x = NULL
#'                   : succeed(x[1]) (x[-1]) when b(x[1]])
#'              : fail()(x)             otherwise
#' ```
#'
#' @details
#' For example, applying the parser `(literal 'a' %then% literal 'b')` to the
#' input `'abcd'` gives the result `[(('a','b'),'cd')]`. Then then combinator
#' is an excellent example of list comprehension notation, analogous to set
#' comprehension in mathematics (e.g.
#' \eqn{\{x^2 | x \in \mathbb{N} \land x \leq 10\}} defines the first ten
#' squares), except that lists replace sets, and elements are drawn in a
#' determined order. Much of the elegance of the then combinator would be lost
#' if this notation were not available.
#'
#' @inheritParams %or%
#' @returns A parser
#' @export
#' @seealso The element-discarding versions [%xthen%] and [%thenx%]
#' @examples
#' starts_with_a <- function(x) grepl("^a",x[1])
#' starts_with_b <- function(x) grepl("^b",x[1])
#' (satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("ab", "bc", "de")) # success
#' (satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("bb", "bc", "de")) # failure
#' (satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("ab", "ac", "de")) # failure
`%then%` <- function(p1, p2) {
  function(x) {
    # Fail on NULL input, otherwise we create endless loops
    if (is.empty(x)) fail()(x)
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
#' The `%using%` combinator allows us to manipulate results from a parser, for
#' example building a parse tree. The parser `(p %using% f)` has the same
#' behavior as the parser `p`, except that the function `f` is applied to each
#' of its result values.
#'
#' @section Formal description:
#'
#' `(p %using$% f) inp = [(fv, out) | (v, out) <- p inp]`
#'
# TODO how to interpret the above code
#'
#' @inheritParams zero.or.more
#' @param f A function to be applied to the result of a succesful `p`
#' @returns A parser
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
#' @section Definitions:
#'
#' `%xthen% <- function(p1, p2) {(p1 %then% p2) %using% fst}`
#'
#' `%thenx% <- function(p1, p2) {(p1 %then% p2) %using% snd}``
#'
#' @inheritParams %or%
#'
#' @return A parser
#' @export
#' @seealso [fst()]
#'
#' @examples
#' is_number <- function(x) grepl("\\d+",x[1])
#' # Numbers are preceded by ">" symbols, but we only need the number itself
#' (literal(">") %thenx% satisfy(is_number)) (c(">", "12"))
#' # Temperatures are followed by the unit 'C', but we only need the number
#' (satisfy(is_number) %xthen% literal("C")) (c("21", "C"))
`%xthen%` <- function(p1, p2) {
  (p1 %then% p2) %using% fst
}

#' @rdname grapes-xthen-grapes
#' @export
#' @seealso [%then%]
`%thenx%` <- function(p1, p2) {
  (p1 %then% p2) %using% snd
}

#' Return a fixed value upon successful parsing.
#'
#' @description
#' Sometimes we are not interested in the result from a parser at all, only
#' that the parser succeeds. For example, if we find a reserved word during
#' lexical analysis, it may be convenient to return some short representation
#' rather than the string itself. The `%ret%` combinator is useful in such
#' cases. The parser `(p %ret% v)` has the same behavior as `p`, except that it
#' returns the value `v` if successful
#'
#' This parser is identical to `p %using% (function(x) {return(c)})`. You may
#' sometimes want to use [%using%] itself for more flexibility.
#'
#' @section Definition:
#'
#' `%ret% <- function(p, c) {p %using% function(x) c}`
#'
#' @inheritParams zero.or.more
#' @param c A single-element character value. Character values are enforced
#'
#' @returns A parser
#' @export
#'
#' @seealso [%using%]
#' @examples
#' (literal("A") %ret% ("We have an A!")) (LETTERS[1:5])
`%ret%` <- function(p, c) {
  p %using% function(x) {return(c)}
}

## Parsers that quantify a parser.

#' Parsers that quantify a parser.
#'
#' @param p A parser
#'
#' @section Formal description:
#'
#' `zero.or.more p = ((p %then% zero.or.more p) %using% cons) %alt% (succeed [])`
#'
#' where `cons` is the list constructor: `cons (x,xs) = x:xs`
#'
#' `one.or.more p = (p %then% zero.or.more p) %using% cons.`
#'
#' Note that these parsers correspond to the `many` (`zero.or.more`) and
#' `some` (`one.or,more`) parsers described in Hutton. The names used here are
#' more clear about what we expect.
#'
#' @returns A parser
#' @export
#' @examples
#' zero.or.more(literal("A")) (c("A",LETTERS[1:5]))
#' zero.or.more(literal("A")) (LETTERS[2:5])
zero.or.more <- function(p) {
  (p %then% zero.or.more(p)) %or% succeed(NULL)
}

#' @rdname zero.or.more
#' @export
#' @examples
#' one.or.more(literal("A")) (c("A",LETTERS[1:5])) # success
#' one.or.more(literal("A")) (LETTERS[2:5]) # failure
#'
one.or.more <- function(p) {
  p %then% zero.or.more(p)
}

#' @rdname zero.or.more
#' @param n An integer
#' @export
#' @examples
#' exactly(2,literal("A")) (c("A", LETTERS[1:5])) # success
#' exactly(2,literal("A")) (c(rep("A",2), LETTERS[1:5])) # failure: too many "A"
#'
exactly <- function(n, p) {
  # notice that this is a greedy parser due to zero.or.more's greediness
  # The non-greedy version is match.n
  function(x) {
    r <- zero.or.more(p)(x)
    if (length(r$L) == n) r else fail()(x)
  }
}

#' @rdname zero.or.more
#' @export
#' @examples
#' zero.or.one(literal("A")) (LETTERS[2:5]) # success
#' zero.or.one(literal("A")) (LETTERS[1:5]) # success
#' zero.or.one(literal("A")) (c("A",LETTERS[1:5])) # failure
#'
zero.or.one <- function(p) {
  exactly(1,p) %or% exactly(0,p)
}

#' @rdname zero.or.more
#' @param n An integer
#' @export
#' @examples
#' match.n(2,literal("A")) (c("A", LETTERS[1:5])) # success
#' match.n(2,literal("A")) (c(rep("A",2), LETTERS[1:5])) # success
#'
match.n <- function(n, p) {
  # non-greedy version of 'exactly'
  if (n == 1) p else (p %then% match.n(n - 1, p))
}

#' The parser that identifies a string and produces custom output.
#'
#' @description
#' `match.s` matches a string using a function and returns a desired object type.
#'
#' @details
#' The function `s` should take a character vector as its single argument. It
#' can return any object when succeeding, but to signal to the parser that it
#' has failed it must return `list()` as output when failing. When constructing
#' the output you should realize that the function will be given a single-
#' element character vector (a string). This often simplifies further
#' processing.
#'
#' This parser short-cuts the pattern `satisfy(b) %using% f`. With `match.s`
#' you do not have to write separate predicate and processing functions `b` and
#' `f` when identification and parsing can be done with a single string
#' parsing function `s`.
#'
#' @param s A string-parsing function.
#' @export
#' @examples
#' want_integers <- function(x) {
#'   m <- gregexpr("[[:digit:]]+", x)
#'   matches <- as.numeric(regmatches(x,m)[[1]])
#'   if (length(matches)==0) {
#'     return(list())
#'   } else {
#'     return(matches)
#'   }
#' }
#' match.s(want_integers) ("12 15 16 and some text") # success
#' match.s(want_integers) ("some text") # failure
#'
match.s <- function(s) {
  function(x) {
    if (is.empty(x)) {
      l <- x
      r <- x
    } else {
      l <- s(x[1])
      r <- x[-1]
    }
    if (failed(l)) fail()(x) else succeed(l)(r)
  }
}
