package week6

/**
  * Created by grmontpetit on 2017-09-24.
  */
object Lecture62 {

  def main(args: Array[String]): Unit = {
    Pair.apply
    println(scalaProduct(List(1, 2, 3), List(1, 5, 7)))
  }

  object Pair {
    val n = 7
    val pairs = (1 until n).map(i => (1 until i).map(j => (i, j)))
    def apply = println(pairs)
  }

  def scalaProduct(xs: List[Double], ys: List[Double]): Double = {
    xs.zip(ys).map(x => x._1 * x._2).foldRight(0.0)((z, i) => z + i)
  }

  object nqueens {
    def isSafe(col: Int, queens: List[Int]): Boolean = {
      val row = queens.length
      val queensWithRow = (row - 1 to 0 by -1) zip queens
      queensWithRow forall {
        case (r, c) => col != c && math.abs(col - c) != row -r
      }
    }
    def queens(n: Int): Set[List[Int]] = {
      def placeQueens(k: Int): Set[List[Int]] =
        if (k == 0) Set(List())
      else
          for {
            queens <- placeQueens(k - 1)
            col <- 0 until n
            if isSafe(col, queens)
          } yield col :: queens
      placeQueens(n)
    }
    def show(queens: List[Int]) = {
      val lines =
        for (col <- queens.reverse)
          yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
      "\n" + lines.mkString("\n")
    }
  }
}
