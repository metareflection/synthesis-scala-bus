trait Dsl {
  val opcodes: List[String]
  val types: List[String]
  lazy val opcodeCache: Set[String] = opcodes.toSet

  def execute(opcode: String, args: List[Any]): Any
  def types(opcode: String): (String, List[String])
  def inferType(v: Any): String
  // TODO: extractConstants
}

trait DslBottomUpSearch extends Dsl with bus.BottomUpSearch {
  type V = Any
  override def evalExpr(e: V): V = {
    assert(!e.isInstanceOf[List[Any]]) // simplif
    e
  }
  override def formalExpr(s: String): V = List("input", s)
  case class DslOp(opcode: String) extends Op {
    override val name = opcode
    val typeSig = types(opcode)
    val returnType = typeSig._1
    val argTypes = typeSig._2
    override val arity = argTypes.length
    override def applicableFromList(vs: List[V]): Boolean =
      arity == vs.length && vs.zip(argTypes).forall{case (v,t) => inferType(v)==t}
    override def computeExprFromList(es: List[V]): V = List(opcode, es)
    override def computeValFromList(vs: List[V]): V = execute(opcode, vs)
  }
}

trait ArithDsl extends Dsl {
  override val opcodes = List("add", "mul", "div", "neg", "lt", "if")
  override val types = List("int", "bool")
  override def execute(opcode: String, args: List[Any]): Any = opcode match {
    case "add" => args(0).asInstanceOf[Int] + args(1).asInstanceOf[Int]
    case "mul" => args(0).asInstanceOf[Int] * args(1).asInstanceOf[Int]
    case "div" => args(0).asInstanceOf[Int] / args(1).asInstanceOf[Int]
    case "neg" => - args(0).asInstanceOf[Int]
    case "lt" => args(0).asInstanceOf[Int] < args(1).asInstanceOf[Int]
    case "if" => if (args(0).asInstanceOf[Boolean]) args(1) else args(2)
  }
  override def types(opcode: String): (String, List[String]) =
    if (Set("neg").contains(opcode))
        ("int", List("int"))
    else if (Set("add", "mul", "div").contains(opcode))
        ("int", List("int", "int"))
    else if (Set("lt").contains(opcode))
        ("bool", List("int", "int"))
    else if (Set("if").contains(opcode))
        ("int", List("bool", "int", "int"))
    else assert(false)
  override def inferType(v: Any): String =
    if (v.isInstanceOf[Int]) "int"
    else if (v.isInstanceOf[Boolean]) "bool"
    else assert(false)
}
