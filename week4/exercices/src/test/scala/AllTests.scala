import lists.Sorting
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import week4.patternmatching.{Number, Prod, Sum, Var}

@RunWith(classOf[JUnitRunner])
class AllTests extends FunSuite {

  test("Test Expr product associativity") {
    val expression = Sum(Prod(Number(2), Var("x")), Var("y"))
    assert(expression.toString == "2 * x + y")
    val expression2 = Prod(Sum(Number(2), Var("x")), Var("y"))
    assert(expression2.toString == "(2 + x) * y")
    val expression3 = Prod(Sum(Number(2), Var("x")), Sum(Number(3), Var("y")))
    assert(expression3.toString == "(2 + x) * (3 + y)")
  }

  test("Test the sorting function") {
    val someList = List(4, 2, 7, 7, 9, 1)
    val sorted = Sorting.isort(someList)
    assert(sorted == List(1, 2, 4, 7, 7, 9))
  }

}
