object Main extends App {
  println("This is week 5")

  val myList = List('a', 'b', 'c', 'd')

  println(last(myList))
  println(init(myList))

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

}
