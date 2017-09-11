package week5

object Lecture52 extends App {

  val list1 = List(9, 3, 8, 5, 10, 20)
  println(msort(list1))

  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (a :: as, z :: zs) =>
          if (a > z) z :: List(a) ::: merge(as, zs)
          else a :: List(a) ::: merge(as, zs)
        case (List(), zs)       => zs
        case (zs, List())       => zs
        case (List(), List())   => xs ::: ys
      }
      val (fst, snd) = xs.splitAt(n)
      merge(msort(fst), msort(snd))
    }
  }
}
