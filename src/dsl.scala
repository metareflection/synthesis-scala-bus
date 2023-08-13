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
  override lazy val ops = opcodes.map(DslOp.apply)
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

object arith_dsl_bus extends DslBottomUpSearch with ArithDsl {
  override def extractConstants(ios: List[IOEx]): List[Piece] = {
    val alwaysConstants = List(0, 1)
    val outputConstants = Nil//ios.map{io => io.output}
    (alwaysConstants ++ outputConstants).map{c =>
      Piece(c, 1, ios.map{_ => c})}
  }
}

trait StringDsl extends Dsl {
  val fewOps = false
  val fewOpcodes = List("Concatenate", "Left", "Right", "If", "Exact")
  val fullOpcodes = List(
    "Concatenate",
    "Left",
    "Right",
    "Mid",
    "Replace",
    "Trim",
    "Repeat",
    "Substitute",
    "SubstituteI",
    "To_Text",
    "Lower",
    "Upper",
    "Proper",
    "If",
    "Add",
    "Minus",
    "Divide",
    "Find",
    "FindI",
    "Len",
    "Exact",
    "Equals",
    "GT",
    "GE",
    "IsNumber",
    "Value")
  override val types = List("str", "int", "bool")
  override def execute(opcode: String, x: List[Any]): Any = opcode match {
    case "Concatenate" => x(0).asInstanceOf[String] + x(1).asInstanceOf[String]
    case "Left" => x(0).asInstanceOf[String].substring(0, x(1).asInstanceOf[Int])
    case "Right" => {
      val x0 = x(0).asInstanceOf[String]
      x0.substring(x0.length - x(1).asInstanceOf[Int], x0.length)
    }
    case "Mid" => {
      val x1 = x(1).asInstanceOf[Int]
      x(0).asInstanceOf[String].substring(x1, x1 + x(2).asInstanceOf[Int])
    }
    case "Replace" => {
      val a = x(0).asInstanceOf[String]
      val start = x(1).asInstanceOf[Int]
      val length = x(2).asInstanceOf[Int]
      val r = x(3).asInstanceOf[String]
      a.substring(0, start) + r + a.substring(start + length, a.length)
    }
    case "Trim" => x(0).asInstanceOf[String].strip()
    case "Repeat" => x(0).asInstanceOf[String] * x(1).asInstanceOf[Int]
    case "Substitute" => x(0).asInstanceOf[String].replace(x(1).asInstanceOf[String], x(2).asInstanceOf[String])
    case "SubstituteI" => x(0).asInstanceOf[String].replace(x(1).asInstanceOf[String], x(2).asInstanceOf[String]) //TODO: , x(3).asInstanceOf[Int])
    case "To_Text" => x(0).toString
    case "Lower" => x(0).asInstanceOf[String].toLowerCase()
    case "Upper" => x(0).asInstanceOf[String].toUpperCase()
    case "Proper" => x(0).asInstanceOf[String].capitalize
    case "If" => if (x(0).asInstanceOf[Boolean]) x(1) else x(2)
    case "Add" => x(0).asInstanceOf[Int] + x(1).asInstanceOf[Int]
    case "Minus" => x(0).asInstanceOf[Int] - x(1).asInstanceOf[Int]
    case "Divide" => x(0).asInstanceOf[Int] / x(1).asInstanceOf[Int]
    case "Find" => x(1).asInstanceOf[String].indexOf(x(0).asInstanceOf[String])
    case "FindI" => x(1).asInstanceOf[String].indexOf(x(0).asInstanceOf[String], x(2).asInstanceOf[Int])
    case "Len" => x(0).asInstanceOf[String].length
    case "Exact" => x(0) == x(1)
    case "GT" => x(0).asInstanceOf[Int] > x(1).asInstanceOf[Int]
    case "GE" => x(0).asInstanceOf[Int] >= x(1).asInstanceOf[Int]
    case "IsNumber" => x(0).asInstanceOf[String].forall(Character.isDigit)
    case "Value" => x(0).asInstanceOf[String].toInt
  }
  override def types(op: String): (String, List[String]) = {
    val s = "str"
    val i = "int"
    val b = "bool"
    if (op == "Concatenate")
      (s, List(s, s))
    else if (Set("Left", "Right").contains(op))
      (s, List(s, i))
    else if (op == "Mid")
      (s, List(s, i, i))
    else if (op == "Replace")
      (s, List(s, i, i, s))
    else if (op == "Trim")
      (s, List(s))
    else if (op == "Repeat")
      (s, List(s, i))
    else if (op == "Substitute")
      (s, List(s, s, s))
    else if (op == "SubstituteI")
      (s, List(s, s, s, i))
    else if (op == "To_Text")
      (s, List(i))
    else if (Set("Lower", "Upper", "Proper").contains(op))
      (s, List(s))
    else if (op == "If")
      (s, List(b, s, s))
    else if (Set("Add", "Minus", "Divide").contains(op))
      (i, List(i, i))
    else if (op == "Find")
      (i, List(s, s))
    else if (op == "FindI")
      (i, List(s, s, i))
    else if (op == "Len")
      (i, List(s))
    else if (op == "Exact")
      (b, List(s, s))
    else if (Set("Equals", "GT", "GE").contains(op))
      (b, List(i, i))
    else if (op == "IsNumber")
      (b, List(s))
    else if (op == "Value")
      (i, List(s))
    else assert(false)
  }
}
