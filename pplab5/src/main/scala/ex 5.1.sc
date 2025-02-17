class Animal(name: String) {
  override def toString: String = name
}
case class Cat(name: String) extends Animal(name)
case class Dog(name: String) extends Animal(name)

// ex 5.1.1

class Carrier[T <: Animal](val content: T)

// ex 5.1.2

val catCarrier: Carrier[Cat] = new Carrier[Cat](Cat("Felix")) // va afisa adresa de memorie a lui Felix
//val animalCarrier: Carrier[Animal] = catCarrier // noi i-am dat Carrier[Cat], dar trebuia sa fie Carrier[Animal]
//val dogCarrier: Carrier[Dog] = new Carrier[Dog](Cat("Merv")) // nu poti crea un Carrier[Dog] cu un Cat in interior

// ex 5.1.3
class Vet[T <: Animal] {
  def treat(patient: T): String = {
    "Can treat " + patient.toString
  }
}

val generalVet: Vet[Animal] = new Vet[Animal]
//val dogVet: Vet[Dog] = generalVet // generalVet e de tip Vet[Animal], dar noi am creat un Vet[Dog]
//dogVet.treat(Cat("Bob")) // nu exista pisici in DogVet
//val catVet: Vet[Cat] = new Vet[Dog] // nu putem crea un Vet[Cat] cu un Vet[Dog]

// ex 5.1.4


enum Ord:
  case LT, EQ, GT

class Comparator[T <: Animal] {
  def compare(o1: T, o2: T): Ord = {
    if (o1.toString < o2.toString) Ord.LT
    else if (o1.toString > o2.toString) Ord.GT
    else Ord.EQ
  }
}
//ex 5.1.5
def detFirst(lst: List[Animal]): Animal = {
  val comparator = new Comparator[Animal]
  lst.reduceLeft((a1, a2) => if (comparator.compare(a1, a2) == Ord.LT) a1 else a2)
}

detFirst(List(Cat("Felix"), Dog("Rex"), Cat("Merv"), Dog("Bob")))

// ex 5.1.6

def apply(g: List[Animal] => Animal)(lst: List[Animal]): Animal = {
  g(lst)
}


