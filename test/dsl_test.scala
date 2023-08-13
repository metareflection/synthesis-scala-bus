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
    assertResult(Some(List("add", List(List("neg", List(1)), List("input", "x")))))(bottomup(List("x"),
      List(IOEx(List(1), 0), IOEx(List(2), 1))))
  }
  // TODO: more tests that require constants
  /*   assert ("if", [("lt", [("input", 0), ("input", 1)]), 1, 0]) == bustle(
   al, int3, [[1, 2, 3], [3, 1, 2]], [1, 0, 0], llProps
   )
   */
}
