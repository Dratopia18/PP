type Str = List[Char]
val l: List[Str] = List("matei@gmail.com", "mihai@gmail.com", "tEst@mail.com", "email@email.com", "short@ax.ro").map(x => x.toList)

def howMany(k: Int)(list: List[Str]): Int = list.foldRight(0)((x, acc) => if (x.length > k) acc + 1 else acc)

howMany(10)(l)