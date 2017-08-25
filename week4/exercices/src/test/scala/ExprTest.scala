import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import week4.patternmatching.{Number, Prod, Sum, Var}

@RunWith(classOf[JUnitRunner])
class ExprTest extends FunSuite {

  test("Test Expr product associativity") {
    val expression = Sum(Prod(Number(2), Var("x")), Var("y"))
    assert(expression.toString == "2 * x + y")
    val expression2 = Prod(Sum(Number(2), Var("x")), Var("y"))
    assert(expression2.toString == "(2 + x) * y")
    val expression3 = Prod(Sum(Number(2), Var("x")), Sum(Number(3), Var("y")))
    assert(expression3.toString == "(2 + x) * (3 + y)")
  }
}
