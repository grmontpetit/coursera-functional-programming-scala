object Lecture61 extends App {
  println("This is week 6")

  val primes = List(2, 3, 4, 5, 7, 11, 13, 15, 17)
  primes.foreach(x => println(s"$x is prime ${isPrime(x)}"))

  def isPrime(nb: Int): Boolean = nb match {
    case 1 => false
    case 2 => true
    case _ => !(2 until nb - 1).exists(x => nb % x == 0)
  }

}
