def realtrycatch(t : => Int, c: => Int): Int  = {
  if (t == 0) c
  else t
}
realtrycatch(throw new Exception, 1)