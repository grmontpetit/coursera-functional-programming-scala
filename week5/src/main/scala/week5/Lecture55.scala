package week5

/**
  * Created by grmontpetit on 2017-09-13.
  */
object Lecture55 extends App {

  val list = List(1, 2, 3, 4) // sum should be: 10
  println(sum(list))
  println(sum2(list))
  println(sum3(list))
  println(sum4(list)) // should print 6

  def sum(xs: List[Int]) = (0 :: xs).reduceLeft((x, y) => x + y)

  def sum2(xs: List[Int]) = xs.reduceRight((x, y) => x + y)

  def sum3(xs: List[Int]) = xs.foldLeft(0)((z, i) => z + i)

  def sum4(xs: List[Int]) =
    xs.foldLeft(0)((z, i) => if (i != 4) z + i else z)

  def sum5(xs: List[Int]) = xs.foldRight(0)((z, i) => z + i)
}
