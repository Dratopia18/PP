type Str = List[Char]
val l: List[Str] = List("matei@gmail.com", "mihai@gmail.com", "tEst@mail.com", "email@email.com", "short@ax.ro").map(x => x.toList)

def mySplit(l: Str, sep: Char): List[Str] = l.foldRight(List(List[Char]()))((c, acc) => if (c == sep) List() :: acc else (c :: acc.head) :: acc.tail).filter(_.nonEmpty)

mySplit("ana are mere".toList, ' ')