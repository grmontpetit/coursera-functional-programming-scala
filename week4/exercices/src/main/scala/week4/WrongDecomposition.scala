trait Expr {
  def isNumber: Boolean // Classification
  def isSum: Boolean // Classification
  def numValue: Int // Accessor
  def leftOp: Expr // Accessor
  def rightOp: Expr // Accessor
}

class Number(n: Int) extends Expr {
  override def isNumber: Boolean = true
  override def numValue: Int = n
  override def isSum: Boolean = false
  override def leftOp: Expr = throw new Error("Number.leftOp")
  override def rightOp: Expr = throw new Error("Number.rightOp")
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  override def isNumber: Boolean = false
  override def rightOp: Expr = e2
  override def numValue: Int = throw new Error("Sum.numValue")
  override def isSum: Boolean = true
  override def leftOp: Expr = e1
}

object test {

  def main(args: Array[String]): Unit = {

  }

  def eval(e: Expr): Int = {
    if (e.isNumber) e.numValue
    else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
    else throw new Error("Unknown expression " + e)
  }
}
