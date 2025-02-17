import scala.annotation.tailrec

def subtractRange2(x: Int, start: Int, stop: Int): Int = {
  if (start > stop) x
  else start - subtractRange2(x, start + 1, stop)
}

subtractRange2(10, 3, 5)