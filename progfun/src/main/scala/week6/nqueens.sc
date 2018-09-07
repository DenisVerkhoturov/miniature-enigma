object nqueens {

  def isSafe(column: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r, c) => column != c && math.abs(column - c) != row - r
    }
  }

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] =
      if (k == 0) Set(List.empty)
      else for {
        queens <- placeQueens(k - 1)
        column <- 0 until n
        if isSafe(column, queens)
      } yield column :: queens

    placeQueens(n)
  }

  def show(queens: List[Int]): String =
    queens.reverse
      .map { Vector.fill(queens.length)("*").updated(_, "X").mkString(" ")}
      .mkString("\n")

  queens(4) map show mkString("\n", "\n", "\n")
}
