# parcr 0.3.0

* Added function `eof()` which detects an end of file. With that, the behavior
  of `satisfy(b)` and `match_s(s)` has been modified: they now fail on empty 
  input, regardless of the functions `b()` and `s()`.

# parcr 0.2.5

* Initial Github submission.
