type Str = List[Char]
type Gradebook = List[(Str,Int)] //the type Gradebook now refers to a list of pairs of String and Int
val gradebook: List[(Str, Int)] = List((List('G'),3), (List('F'), 10), (List('M'),6), (List('P'),4), (List('A'),7))
def increment(g: Gradebook, p: (Str, Int) => Boolean): Gradebook =
  g.map {
    case (s, gr) if p(s, gr) => (s, gr + 1)
    case (s, gr) => (s, gr)
  }

increment(gradebook, (s, gr) => s.length == 1 && gr < 5)

def average(g: Gradebook): Double = g.foldRight((0, 0)) {
  case ((_, gr), (sum, count)) => (sum + gr, count + 1)
} match {
  case (sum, count) => sum.toDouble / count
}

average(gradebook)

def pass(g: Gradebook): List[Str] = g.filter {
  case (s, gr) => gr >= 5
}.map {
  case (s, _) => s
}

pass(gradebook)

def mergeSort(l: Gradebook): Gradebook = {
  def merge(u: Gradebook, v: Gradebook): Gradebook = (u, v) match {
    case (Nil, _) => v
    case (_, Nil) => u
    case ((s1, gr1) :: t1, (s2, gr2) :: t2) =>
      if (gr1 < gr2) (s1, gr1) :: merge(t1, v)
      else (s2, gr2) :: merge(u, t2)
  }
  if (l.length <= 1) l
  else {
    val (u, v) = l.splitAt(l.length / 2)
    merge(mergeSort(u), mergeSort(v))
  }
}

mergeSort(gradebook)

def honorsList(g: Gradebook): List[Str] = g.filter {
  case (_, gr) => gr >= 5
} .sortWith {
  case ((_, gr1), (_, gr2)) => gr1 > gr2
}.map {
  case (s, _) => s
}

honorsList(gradebook)