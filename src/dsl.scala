trait Dsl {
  val ops: List[String]
  val types: List[String]
  lazy val opCache: Set[String] = ops.toSet

  def execute(op: String, args: List[Any]): Any
  def types(op: String): (String, List[String])
  def inferTypes(v: Any): String
  // TODO: extractConstants
}

trait DslBottomUpSearch extends bus.BottomUpSearch {
  type V = Any
  override def evalExpr(e: V): V = {
    assert(!e.isInstanceOf[List[Any]]) // simplif
    e
  }
  override def formalExpr(s: String): V = List("input", s)
}

trait ArithDsl extends Dsl {
  override val ops = List("add", "mul", "div", "neg", "lt", "if")
  override val types = List("int", "bool")
  override def execute(op: String, args: List[Any]): Any = op match {
    case "add" => args(0).asInstanceOf[Int] + args(1).asInstanceOf[Int]
    case "mul" => args(0).asInstanceOf[Int] * args(1).asInstanceOf[Int]
    case "div" => args(0).asInstanceOf[Int] / args(1).asInstanceOf[Int]
    case "neg" => - args(0).asInstanceOf[Int]
    case "lt" => args(0).asInstanceOf[Int] < args(1).asInstanceOf[Int]
    case "if" => if (args(0).asInstanceOf[Boolean]) args(1) else args(2)
  }
  override def types(op: String): (String, List[String]) =
    if (Set("neg").contains(op))
        ("int", List("int"))
    else if (Set("add", "mul", "div").contains(op))
        ("int", List("int", "int"))
    else if (Set("lt").contains(op))
        ("bool", List("int", "int"))
    else if (Set("if").contains(op))
        ("int", List("bool", "int", "int"))
    else assert(false)
  override def inferTypes(v: Any): String =
    if (v.isInstanceOf[Int]) "int"
    else if (v.isInstanceOf[Boolean]) "bool"
    else assert(false)
}
