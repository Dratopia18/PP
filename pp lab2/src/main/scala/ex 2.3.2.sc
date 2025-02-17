def compare(x: Int)(y: Int)(z: Int): Int =
{
  if (x > y && x > z) x
  else if (y > x && y > z) y
  else z
}

compare(1)(2)(1)