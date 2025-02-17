def sumNats(start: Int, stop: Int): Int = {
  if (start > stop) 0
  else start + sumNats(start + 1, stop)
}

sumNats(1, 5)
def tailSumNats(start: Int, stop: Int): Int = {
  def auxSumNats(current: Int, acc: Int): Int = {
    if (current > stop) acc
    else current + auxSumNats(current + 1, acc)
  }
  auxSumNats(start, 0)
}
tailSumNats(1, 5)