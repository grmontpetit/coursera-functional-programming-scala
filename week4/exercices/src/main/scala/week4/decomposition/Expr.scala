trait Expr {
  def eval: Int
}

class number(n: Int) extends Expr {
  def eval: Int = n
}

class Sum(e1: Expr, e2: Expr) extends Expr {
  override def eval: Int = e1.eval + e2.eval
}
