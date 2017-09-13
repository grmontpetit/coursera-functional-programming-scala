package week5

object Lecture54 extends App {

  val myList = List(1, 2, 3)
  println(squareList(myList))
  println(squareList2(myList))

  val p = List("a","a","a","b","c","c","a")
  println(pack(p))
  println(p.span(x => x == "a"))
  println(encode(p))
  println(encode2(p))

  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil     => xs
    case y :: ys => (y * y) :: squareList(ys)
  }

  def squareList2(xs: List[Int]): List[Int] = xs.map(x => x * x)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil     => Nil
    case x :: xs1 =>
      val (first, rest) = xs.span(y => y == x)
      first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[Map[T, Int]] = {
    pack(xs).map(l => l.groupBy(identity).mapValues(_.size))
  }

  def encode2[T](xs: List[T]): List[(T, Int)] = {
    pack(xs).map(ys => (ys.head, ys.length))
  }
}
