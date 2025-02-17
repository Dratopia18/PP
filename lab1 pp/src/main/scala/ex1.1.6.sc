import scala.annotation.tailrec

def subtractRange(x: Int, start: Int, stop: Int): Int = {
  @tailrec
  def subtractHelper(currX: Int, currIndex: Int): Int = {
    if(currIndex > stop) currX
    else subtractHelper(currX - currIndex, currIndex + 1)
  }
  subtractHelper(x - start, start + 1)
}

subtractRange(10, 3, 5)