% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsers_basic.R
\name{zero_or_more}
\alias{zero_or_more}
\alias{one_or_more}
\alias{exactly}
\alias{zero_or_one}
\alias{match_n}
\title{Parsers that repeat application of a parser.}
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
A parser
}
\description{
Often, a structure that can be parsed by a parser \code{p} is repeated several
times in a file, or the parser can be applied zero or more times. The
following combinators test these conditions.
}
\details{
All these parsers except \code{match_n} are greedy. This means that they try to
apply \code{p} as many times as possible. If that number of times is not equal to
what was expected, for example if \code{p} can successfully parse more than \code{n}
times in \code{exactly(n,p)}, then the parser will fail. In contrast,
\code{match_n(n,p)} will apply \code{p} \code{n} times and no more, even if \code{p} could be
successfully applied more often. Clearly, both functions will fail if \code{p}
leads to failure after less than \code{n} repetitions.
}
\section{Pseudocode}{

\preformatted{
zero_or_more(p):
  (p \%then\% zero_or_more(p)) \%or\% succeed(null)

one_or_more(p):
  p \%then\% zero_or_more(p)

exactly(n,p):
  count = 0
  r = zero_or_more((p %using% F(x): count = count + 1; x))(x)
  if counter == n then
    count = 0
    r
  else fail()(x)

zero_or_one:
  exactly(0,p) \%or\% exactly(1,p)

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
