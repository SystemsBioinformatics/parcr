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
