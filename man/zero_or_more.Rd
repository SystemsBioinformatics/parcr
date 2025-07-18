% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsers_basic.R
\name{zero_or_more}
\alias{zero_or_more}
\alias{one_or_more}
\alias{exactly}
\alias{zero_or_one}
\alias{match_n}
\title{Repeated application of a parser}
\usage{
zero_or_more(p)

one_or_more(p)

exactly(n, p)

zero_or_one(p)

match_n(n, p)
}
\arguments{
\item{p}{a parser.}

\item{n}{a positive integer, including 0.}
}
\value{
A parser.
}
\description{
Often, we want to assess whether a given structure can be successfully
parsed through repetitive application of a parser \code{p}. This could involve
testing the parser applied multiple times in succession or determining
its capability to be applied zero or more times.

The subsequent functions are designed to address and evaluate these
scenarios.
}
\details{
All these parsers with the exception of \code{match_n} exhibit greedy behavior
striving to apply \code{p} as many times as possible. If the resulting count
doesn't match the expected quantity, such as in the case of \code{exactly(n,p)}
where \code{p} successfully parses more than \code{n} times, then the parser fails.
In contrast, \code{match_n(n,p)} strictly applies \code{p} exactly \code{n} times,
preventing any further application of \code{p} even if \code{p} could potentially be
applied more often. Clearly, both functions will fail if \code{p} fails after
less than \code{n} repetitions.
}
\section{Pseudocode}{

\preformatted{
zero_or_more(p):
  (p \%then\% zero_or_more(p)) \%or\% succeed(null)

one_or_more(p):
  p \%then\% zero_or_more(p)

exactly(n,p):
  count = 0
  r = zero_or_more(p \%using\% F(x): count = count + 1; x)(x)
  if count == n then
    count = 0
    r
  else fail()(x)

zero_or_one:
  exactly(1,p) \%or\% exactly(0,p)

match_n(n,p):
  if n==0 then F(x): succeed(list())(x)
  else
    if n==1 then p else (p \%then\% match_n(n-1, p))
}

where \code{null} is the empty vector.
}

\examples{
zero_or_more(literal("A")) (c("A",LETTERS[1:5]))
zero_or_more(literal("A")) (LETTERS[2:5])

one_or_more(literal("A")) (c("A",LETTERS[1:5])) # success
one_or_more(literal("A")) (LETTERS[2:5]) # failure

exactly(2,literal("A")) (c("A", LETTERS[1:5])) # success
exactly(2,literal("A")) (c(rep("A",2), LETTERS[1:5])) # failure: too many "A"

zero_or_one(literal("A")) (LETTERS[2:5]) # success
zero_or_one(literal("A")) (LETTERS[1:5]) # success
zero_or_one(literal("A")) (c("A",LETTERS[1:5])) # failure

match_n(2,literal("A")) (c("A", LETTERS[1:5])) # success
match_n(2,literal("A")) (c(rep("A",2), LETTERS[1:5])) # success

}
