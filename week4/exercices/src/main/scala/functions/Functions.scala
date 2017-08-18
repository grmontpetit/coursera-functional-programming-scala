package functions

object Functions extends App {

  val anonymousFunction = (x: Int) => x * x

  val anonClass1 = { class AnonFun extends Function1[Int, Int] {
    def apply(x: Int) = x * x
    }
    new AnonFun
  }

  val anonClass2 = new Function1[Int, Int] {
    def apply(x: Int) = x * x
  }

  println(s"exp2 = ${anonymousFunction(2)}")
  println(s"exp = ${anonClass1(2)}")
  println(s"anonymousClass = ${anonClass2(2)}")
}
