type Str = List[Char]
val l: List[Str] = List("matei@gmail.com", "mihai@gmail.com", "tEst@mail.com", "email@email.com", "short@ax.ro").map(x => x.toList)

def longer(k: Int, list: List[Str]): List[Str] = list.filter(x => x.length < k)

def longer2(k: Int, list: List[Str]): List[Str] = list match {
  case Nil => Nil
  case x :: xs => if (x.length < k) x :: longer2(k, xs) else longer2(k, xs)
}

longer(15, l)

longer2(15, l)