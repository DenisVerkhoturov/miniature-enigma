object polynomials {
  val romanNumerals = Map('I' -> 1, 'V' -> 5, 'X' -> 10)
  val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

  capitalOfCountry get "Andorra"
  capitalOfCountry get "US"

  class Poly(terms0: Map[Int, Double]) {
    val terms: Map[Int, Double] = terms0 withDefaultValue 0.0

    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    def +(other: Poly): Poly = new Poly((other.terms foldLeft terms) (addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exponent, coefficient) = term
      terms + (exponent -> (coefficient + terms(exponent)))
    }

    override def toString: String =
      (for ((exponent, coefficient) <- terms.toList.sorted.reverse) yield s"${coefficient}x^$exponent") mkString " + "
  }

  val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
  val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
  p1 + p2
}
