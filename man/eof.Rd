% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsers_basic.R
\name{eof}
\alias{eof}
\title{Detect end of input.}
\usage{
eof()
}
\value{
A parser
}
\description{
\code{eof} tests whether the end of the input character vector has been reached,
which boils down to detection of \code{character(0)} as input. Since the intended
application of this parser is parsing of text files the function has been
called after the end of file (EOF) signal. To indicate that an end of file
has been detected, the right hand side of the parser result will be
converted to an empty list.
}
\section{Pseudocode}{

\preformatted{
eof()(x):
  if x==null then succeed(x)(list())
  else fail()(x)
}
}

\examples{
(literal("a") \%then\% eof())("a") # success
# Notice the difference on the R-side with
literal("a")("a")
eof()(character(0)) # success
eof()("a") # failure

}
