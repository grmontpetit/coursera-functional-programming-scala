package week4

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  override def tail: List[Nothing] = throw new NoSuchElementException
  override def head: Nothing = throw new NoSuchElementException
}

object test {
  val x: List[String] = Nil
  def f(xs: List[NonEmpty], x: Empty): List[IntSet] = xs.prepend(x)
}
