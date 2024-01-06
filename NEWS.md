# parcr (development version)

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
