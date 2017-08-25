package week4.patternmatching

trait Expr {
  def eval: Int = this match {
    case Number(n)       => n
    case Sum(e1, e2)     => e1.eval + e2.eval
    case Var(_)          => throw new Error("This is a variable")
    case Prod(e1, e2)    => e1.eval * e2.eval
  }
  def show: String = this match {
    case Number(n)       => n.toString
    case Sum(e1, e2)     => e1.show + " + " + e2.show
    case Var(v)          => v
    case Prod(e1, e2)    => e1.show + " * " + e2.show
  }
  override def toString: String = this match {
    case Number(n)            => n.toString
    case Sum(e1, e2)          => e1.toString + " + " + e2.toString
    case Prod(x: Sum, y: Sum) => "(" + x.toString + ") * (" + y.toString + ")"
    case Prod(x: Sum, y)      => "(" + x + ") * " + y.toString
    case Prod(x, y: Sum)      => x.toString + " * (" + y + ")"
    case Var(v)               => v
    case Prod(e1, e2)         => e1.toString + " * " + e2.toString
  }
  def isSum: Boolean = this match {
    case Sum(_, _) => true
    case _         => false
  }
}
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Var(v: String) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

// Scala compiler automatically and implicitly adds companion objects
// to case classe, like :
//object Number {
//  def apply(n: Int) = new Number(n)
//}
//
//object Sum {
//  def apply(e1: Expr, e2: Expr) = new Sum(e1, e2)
//}
