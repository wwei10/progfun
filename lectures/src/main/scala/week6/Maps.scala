package week6

object Maps {

  def main(args: Array[String]): Unit = {
    val romanNumerals = Map('I' -> 1, 'V' -> 5, 'X' -> 10)
    val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")
    println(romanNumerals)
    println(capitalOfCountry)
    println(capitalOfCountry get "US")
    println(capitalOfCountry get "Germany")
    
    def showCapital(country: String) = capitalOfCountry get country match {
      case Some(capital) => capital
      case None => "Missing data"
    }
    println(showCapital("US"))
    println(showCapital("Germany"))
    println(showCapital("Switzerland"))
    
    val fruit = List("apple", "pear", "orange", "pineapple")
    println(fruit sortWith (_.length < _.length))
    println(fruit.sorted)
    println(fruit.groupBy(_.head))
  }

}