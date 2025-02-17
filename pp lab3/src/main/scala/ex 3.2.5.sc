type Str = List[Char]
val l: List[Str] = List("matei@gmail.com", "mihai@gmail.com", "tEst@mail.com", "email@email.com", "short@ax.ro").map(x => x.toList)

def mySplit(l: Str, sep: Char): List[Str] = l.foldRight(List(List[Char]()))((c, acc) => if (c == sep) List() :: acc else (c :: acc.head) :: acc.tail).filter(_.nonEmpty)

// Implement a function that return the domains without the dot (ex. gmail). Use mySplit
def domains(list: List[Str]): List[Str] = list.map(x => mySplit(x, '@').tail.head).map(x => mySplit(x, '.').head)
domains(l)