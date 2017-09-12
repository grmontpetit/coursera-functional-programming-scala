package week5

object Lecture53 extends App {

  val list1 = List(9, 3, 8, 5, 10, 20)
  println(msort(list1))

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (x :: xs1, y :: ys1)   =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
        case (List(), zs)       => zs
        case (zs, List())       => zs
        case (List(), List())   => xs ::: ys
      }
      val (fst, snd) = xs.splitAt(n)
      merge(msort(fst), msort(snd))
    }
  }
}
