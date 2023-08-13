import org.scalatest.funsuite.AnyFunSuite

class ArithDslTesting extends AnyFunSuite {
  import arith_dsl_bus._
  test("arith id") {
    assertResult(Some(List("input", "x")))(bottomup(List("x"),
      List(IOEx(List(1), 1), IOEx(List(2), 2))))
  }
  test("arith inc") {
    assertResult(Some(List("add", List(1, List("input", "x")))))(bottomup(List("x"),
      List(IOEx(List(1), 2), IOEx(List(2), 3))))
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
