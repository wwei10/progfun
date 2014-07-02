package week6

object NQueens {
  
  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
      yield Vector.fill(queens.length)("*").updated(col, "X").mkString
    "\n" + (lines mkString "\n")
  }
  
  def nqueens(n: Int): Set[List[Int]] = {
    def isSafe(col: Int, queen: List[Int]): Boolean = {
      val row = queen.length
      ((row - 1 to 0 by -1) zip (queen)) forall {
        case (r, c) => col != c && row + col != r + c && row - col != r - c
      }
    }
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) {
        Set(List())
      } else {
        val queens = placeQueens(k - 1)
        for {
          queen <- queens
          col <- (0 until n)
          if isSafe(col, queen)
        } yield col :: queen
      }
    }
    placeQueens(n)
  }
  
  def main(args: Array[String]): Unit = {
    println("4 Queens: #" + nqueens(4).size)
    println(nqueens(4))
    println("8 Queens: #" + nqueens(8).size)
    println(nqueens(8))
    println((nqueens(4) map (show)) mkString "\n")
  }

}