# Closely following Graham Hutton's paper https://doi.org/10.1017/S0956796800000411

## Primitive parsers
# The primitive parsers are the building blocks of combinator parsing.

#' The parser that always succeeds
#'
#' @description
#' `succeed` parser always succeeds, without actually consuming any
#' input string. Since the outcome of succeed does not depend on its input, its
#' result value must be pre-determined, so it is included as an extra parameter.
#'
#' @section Formal description:
#'
#' `succeed v inp = [(v, inp)].`
#'
#' @param left The part to be added on the L-side of a parsed list
#' @export
#' @examples
#' succeed(letters[1:5]) (letters[10:15])
succeed <- function(left) {
  function(right) list(L=ensure.list(left), R=right)
}

#' The parser that always fails
#'
#' @description
#' While \code{succeed} parser never fails, \code{fail} always does, regardless
#' of the input vector. It, therefore, returns the empty list.
#'
#' @section Formal description:
#'
#' `fail inp = [].`
#'
#' @export
#' @examples
#' fail()(letters[1:5])
#'
fail <- function() {
  function(cv) list()
}

#' The parser that matches an element using a predicate
#'
#' @description
#' `satisfy` turns a predicate function into a parser that recognizes single
#' elements.
#'
#' @section Formal description:
#'
#' `satisfy b []     = fail []`
#'
#' `satisfy b (x:xs) = succeed x xs , when b x`
#'
#' `                 = fail xs , when not b x`
#'
#' @param b A boolean function to determine if the element is accepted.
#' @export
#' @examples
#'
#' # define a predicate function that tests whether the next element starts
#' # with an 'a'
#' starts_with_a <- function(x) grepl("^a",x[1])
#' # Use it in the satisfy parser
#' satisfy(starts_with_a)(c("abc","def")) # success
#' satisfy(starts_with_a)(c("bca","def")) # failure
satisfy <- function(b) {
  return(
    function(cv) {
      if (is.empty(cv)) {
        r <- list(L=cv, R=cv)
      } else {
        r <- list(L=cv[1], R=cv[-1])
      }
      if (b(r$L)) succeed(r$L)(r$R)
      else fail()(cv)
    })
}

#' The parser that matches an element using a literal string
#'
#' @description
#' `literal` is a parser for single elements. It tests whether the first
#' element in the vector is equal to a given element.
#'
#' @section Formal description:
#'
#' `literal x = satisfy (= x)`
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
    }
  )
}

## Combinators
# Now that we have the basic building blocks, we consider how they should be put
# together to form useful parsers.

#' The alternation combinator
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
  function(cv) {
    r1 <- p1(cv)
    r2 <- p2(cv)
    if (!failed(r1)) r1 else {if (!failed(r2)) r2 else fail()(cv)}
  }
}

#' The sequence parser
#'
#' @description
#'
#' `(p1 %then% p2)` recognizes anything that `p1` and `p2` would if placed in
#' succession.
#'
#' @section Formal description:
#'
#' `(p1 %then% p2) inp = [((v1,v2),out2) | (v1,outl) <- p1 inp;`
#' `                                       (v2,out2) <- p2 out1]`
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
  function(cv) {
    # Fail on character(0) input, otherwise we we create an endless loop
    if (is.empty(cv)) fail()(cv)
    else {
      r1 <- p1(cv)
      if (failed(r1)) fail()(cv)
      else {
        r2 <- p2(r1$R)
        if (failed(r2)) fail()(cv) else
        return(list(L=c(r1$L, r2$L), R=r2$R))
      }
    }
  }
}

#' Manipulate results from a parser by applying a function
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
  function(cv) {
    r <- p(cv)
    if (failed(r)) fail()(cv)
    else return(list(L = ensure.list(f(r$L)), R=r$R))
  }
}

#' Selecting only left or right part from a `%then%` sequence
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

#' Return a given value upon successful parsing
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
#' `%ret% <- function(p, c) {p %using% function(x) as.character(c))}`
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
  p %using% function(x) {return(as.character(c))}
}

## Quantifying parsers

#' Quantifying parsers for p
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
  # notice: c used below is the concatenation function from R
  (p %then% zero.or.more(p)) %or% succeed(character(0))
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
    if (length(r$L) == n) {
      return(r)
    } else {
      return(fail()(x))
    }
  }
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
  if (n == 1) {
    return(p)
  } else {
    return((p %then% match.n(n - 1, p)))
  }
}

## Some common elements

#' @title Recognize an empty line
#'
#' @description
#'
#' An empty line is a line that consists entirely of space-like characters.
#' `Empty.line` is a parser that recognizes one empty line and `Spacer`
#' recognizes one or more empty lines and `MaybeEmpty` recognizes zero or more
#' empty lines. `Empty.line` actually returns the empty line but `Spacer` and
#' `MaybeEmpty` discard the empty lines.
#'
#' @importFrom stringr str_replace_all
#' @export
#'
#' @examples
#' Empty.line() (c(' \t  ')) # success
#' Empty.line() (c('    .')) # failure
Empty.line <- function() {
  satisfy(function(x) {stringr::str_replace_all(x, "\\s+", "") == ""})
}

#' @rdname Empty.line
#' @export
#' @examples
#' Spacer() (c("   \t  ", "    ", "abc"))
#' Spacer() (c("            ", "    ", "Important text"))
Spacer <- function() {
  (one.or.more(Empty.line())) %ret% NULL
}

#' @rdname Empty.line
#' @export
#' @examples
#' MaybeEmpty() (c("   \t  ", "    ", "abc"))
#' MaybeEmpty() (c("            ", "    ", "Important text"))
MaybeEmpty <- function() {
  (zero.or.more(Empty.line())) %ret% NULL
}

#' Extracts all integer and floating point numbers from a line
#'
#' Ignores any other symbols. It tests whether exactly n numbers are found.
#'
#' @param n An integer.
#'
#' @return A parser.
#' @export
#' @examples
#' Numbers(3) ('1  2  3')
#' Numbers(3) ('1101\t201\t33')
#'
Numbers <- function(n) {
    (satisfy( function(x) {
      (stringr::str_extract_all(x, pattern = "[\\d\\.]+", simplify = TRUE) |>
        as.vector() |> length()) == n
      })) %using%
    function(x) {
      stringr::str_extract_all(x, pattern = "[\\d\\.]+", simplify = TRUE) |>
        as.vector() |> as.numeric()
    }
}

