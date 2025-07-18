% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stringparser_constructor.R
\name{stringparser}
\alias{stringparser}
\title{String parser constructor}
\usage{
stringparser(
  match_pattern,
  reshape = function(x) {
     x
 }
)
}
\arguments{
\item{match_pattern}{A regular expression that matches the string.}

\item{reshape}{A function that takes the character vector of captured strings
and modifies it to a desired output. By default this is the identity function.}
}
\description{
Produce a string parser based on \code{\link[stringr:str_match]{stringr::str_match()}}, to be used with
\code{\link[=match_s]{match_s()}}.
}
\details{
This function uses \code{\link[stringr:str_match]{stringr::str_match()}} to produce a string parser. Parsers
created with this constructor return the failure signal \code{list()} when a
string does not match the \code{match_pattern}. If the pattern contains captured
groups then these groups will be returned as a character vector upon matching.
If there is no capture group then the function will return silently upon
matching the pattern. You can provide a function to the \code{reshape} argument
to change the output upon matching.

You always have to wrap the parsers made with this constructor in
\code{match_s()} to create a parser combinator. I decided not to include this
standard pattern in the \code{stringparser} constructor itself because it
complicates testing of these parsers.
}
\examples{
# single capture group
parse_header <- stringparser("^>(\\\\w+)")
parse_header(">correct_header")     # returns "correct_header"
parse_header("> incorrect_header")  # returns list()

# multiple capture groups
parse_keyvalue <- stringparser("(\\\\w+):\\\\s?(\\\\w+)")
parse_keyvalue("key1: value1")      # returns c("key1", "value1")

# modify output
parse_keyvalue_df <- stringparser("(\\\\w+):\\\\s?(\\\\w+)",
                                   function(x) data.frame(key = x[1], value = x[2])
                                 )
parse_keyvalue_df("key1: value1")      # returns a data frame

}
\seealso{
\code{\link[=match_s]{match_s()}}, \code{\link[stringr:str_match]{stringr::str_match()}}
}
