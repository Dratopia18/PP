type Str = List[Char]
val l: List[Str] = List("matei@gmail.com", "mihai@gmail.com", "tEst@mail.com", "email@email.com", "short@ax.ro").map(x => x.toList)
def remUpper(list: List[Str]): List[Str] = list.map(_.filterNot(_.isUpper))

remUpper(l)