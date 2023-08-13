import org.scalatest.funsuite.AnyFunSuite

object dsl_printer {
    def pretty(x: Any): String = x match {
      case List("input", name) => s"input($name)"
      case List(op: String, args: List[Any]) =>
        op + args.map(pretty).mkString("(", ", ", ")")
      case _ => x.toString
  }
}
import dsl_printer._

class ArithDslTesting extends AnyFunSuite {
  import arith_dsl_bus._
  test("arith id") {
    assertResult(Some(List("input", "x")))(bottomup(List("x"),
      List(IOEx(List(1), 1), IOEx(List(2), 2))))
  }
  test("arith id str") {
    assertResult(Some("input(x)"))(bottomup(List("x"),
      List(IOEx(List(1), 1), IOEx(List(2), 2))).map(pretty))
  }
  test("arith inc") {
    assertResult(Some(List("add", List(1, List("input", "x")))))(bottomup(List("x"),
      List(IOEx(List(1), 2), IOEx(List(2), 3))))
  }
  test("arith inc str") {
    assertResult(Some("add(1, input(x))"))(bottomup(List("x"),
      List(IOEx(List(1), 2), IOEx(List(2), 3))).map(pretty))
  }
  test("arith neg") {
    assertResult(Some(List("neg", List(List("input", "x")))))(
      bottomup(List("x"),
        List(IOEx(List(1), -1), IOEx(List(2), -2))))
  }
  test("arith dec") {
    assertResult(Some(List("add", List(List("neg", List(1)), List("input", "x")))))(
      bottomup(List("x"),
        List(IOEx(List(1), 0), IOEx(List(2), 1))))
  }
  test("arith if") {
    assertResult(Some(List("if", List(List("lt", List(List("input", "y"), List("input", "x"))), List("input", "y"), List("input", "x")))))(
      bottomup(List("x", "y"),
        List(IOEx(List(20, 30), 20), IOEx(List(20, 0), 0), IOEx(List(30, 10), 10))))
  }
}

class StringDslTesting extends AnyFunSuite {
  import string_dsl_bus._
  test("test 1") {
    assertResult(Some("Left(input(x), 1)"))(bottomup(List("x"),
      List(IOEx(List("hello"), "h"), IOEx(List("world"), "w"))).map(pretty))
  }
}
