# parcr (development version)

# parcr 0.6.0

## New feature

* Improved error messaging.
    + The line where the parser fails is now displayed together with its context
    + The error message mentions expected values. Some of these are 
      automatically inferred, whereas others can be induced by creating named 
      parsers

## Bug fix

* The function `stringparser()` depended on `stringr::str_match()`. However, 
the dependency was not listed in DESCRIPTION. This may have led to errors when 
the `stringr` package was not installed. The function has been fixed by 
implementing it with base functions.

# parcr 0.5.3

## New feature

* Added a constructor function for string parsers using stringr::str_match() 
because the pattern occurs very often in the construction of string parsers.

# parcr 0.5.2

## Bug fix

* Parser zero_or_one() was not greedy, meaning that it sometimes did not parse
input that it should be able to parse (see 
[issue #12](https://github.com/SystemsBioinformatics/parcr/issues/12)).

## New feature

* Added the function Ignore() which reads and discards all elements until the 
  end of a vector.

# parcr 0.5.1

* Because of the use of base::isa() this package requires R 4.1 or higher, 
  not R 2.1. This has been corrected in the DESCRIPTION. Running on 
  ubuntu-latest and R 4.1 is checked by CI workflow on Github.
* Minor corrections and additions to both vignettes.

# parcr 0.5.0

* All function descriptions and documentation thoroughly reviewed and updated.
* Vignettes reviewed and updated.
* Published on CRAN

# parcr 0.4.5

* Extended the fasta file example in the vignette by showing the use of %or% 
  to apply alternative parsers. Added a protein sequence fasta file to the data
  set.
* Function `Parser()` was changed and renamed to `reporter()`. This better 
  reflects its use.

# parcr 0.4.4

* Added (started) Details vignette.
* Custom `error_parser` includes line number and line content fields of the 
  element index on which the parser failed.
* Added function `finished()` to test for complete consumption of the input by
  the parser.

# parcr 0.4.3

* `print.marker` now emits '[]' to make explicit that it differs from a simple
  `list()`.
* Export of function `failed()` to test failure of a parser. Useful in 
  unit testing.

# parcr 0.4.2

* Resolved [issue #5](https://github.com/SystemsBioinformatics/parcr/issues/5)

# parcr 0.4.1

* Correct example in documentation for Parser() function

# parcr 0.4.0

* Marker error messaging implemented and tested. Markers are line numbers 
  (element numbers in a character vector) where the parser fails. In case of
  alternative parsers like `p1 %or% p2` the marker with the largest index value
  is reported when both parsers fail.

# parcr 0.3.3.9000

* Developing marker error messaging

# parcr 0.3.3

* Again modified `exacly(n,p)` because, since repeater functions call the 
  same instance of a function, the counter is not reset when this 
  parser is nested in a repeater parser (`zero_or_more`, etc.). The reset is 
  now performed in the function itself.

# parcr 0.3.2

* Added `fastafile` example data which is used in the vignette.

# parcr 0.3.1

* Redesigned `exactly(n,p)` to behave as expected with non-emitting parsers 
  like `literal("A") %ret% NULL` and `eof()`.

* Dependencies on packages `stringr` and `methods` were removed.

# parcr 0.3.0

* Added function `eof()` which detects an end of file. With that, the behavior
  of `satisfy(b)` and `match_s(s)` has been modified: they now fail on empty 
  input, regardless of the functions `b()` and `s()`.

# parcr 0.2.5

* Initial Github submission.
