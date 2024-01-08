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
