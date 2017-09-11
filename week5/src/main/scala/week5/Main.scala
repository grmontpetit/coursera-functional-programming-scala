object Main extends App {
  println("This is week 5")

  val myList = List('a', 'b', 'c', 'd')

  println(last(myList))
  println(init(myList))
  println(reverse(myList))
  println(removeAt(myList, 2))

  def last[T](xs: List[T]): T = xs match {
    case List()  => throw new Error("last on empty list")
    case List(x) => x
    case _ :: ys => last(ys)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case List()  => throw new Error("init on empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List()  => ys
    case z :: zs => z :: concat(zs, ys)
  }

  // n * n complexity
  def reverse[T](xs: List[T]): List[T] = xs match {
    case List()  => xs
    case y :: ys => reverse(ys) :+ y
  }

  def removeAt[T](xs: List[T], n: Int): List[T] = xs match {
    case List()       => xs
    case head :: tail =>
      if (n == 0) tail
      else head :: removeAt(tail, n - 1)
  }
}
