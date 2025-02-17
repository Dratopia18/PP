object StringOps {
  implicit class StringExtensions(val str: String) extends AnyVal {
    def <<(n: Int): String = {
      val shift = n % str.length
      str.substring(shift)
    }

    def >>(n: Int): String = {
      val shift = str.length - (n % str.length)
      str.substring(0, shift)
    }

    def >>>(n: Int): String = {
      val shift = str.length - (n % str.length)
      str.substring(shift) + str.substring(0, shift)
    }

    def <<<(n: Int): String = {
      val shift = n % str.length
      str.substring(shift) + str.substring(0, shift)
    }

    def -(sub: String): String = {
      str.replaceAll(sub, "")
    }

    def unary_~ : String = {
      str.map{
        case c if c.isLower => c.toUpper
        case c if c.isUpper => c.toLower
        case c => c
      }
    }
    def <=>(other: String) : Int = {
      str.compareTo(other) match {
        case 0 => 0
        case x if x > 0 => 1
        case _ => -1
      }
    }
    def /(n: Int): List[String] = {
      str.grouped(n).toList
    }
  }
}

import StringOps._

val result1 = "odersky" << 2
val result2 = "odersky" >> 2
val result3 = "odersky" >>> 2
val result4 = "odersky" <<< 2
val result = "Implement the ''-'' operator between two strings that removes occurrences of the second string from the first one:" - "th"
val result5 = ~"xXx1337ScAlAcOdErxXx"
val result6 = "haskell" <=> "java"
val result7 = "scala" <=> "java"
val result8 = "scala" <=> "scala"
val result9 = "scalable language" / 3