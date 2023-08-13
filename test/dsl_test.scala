import org.scalatest.funsuite.AnyFunSuite

class ArithDslTesting extends AnyFunSuite {
  import arith_dsl_bus._
  test("arith id") {
    assertResult(Some(List("input", "x")))(bottomup(List("x"),
      List(IOEx(List(1), 1), IOEx(List(2), 2))))
  }
  ignore("arith inc") {
    // TODO: need to extractConstants
    assertResult(Some(List("add", List("x", 1))))(bottomup(List("x"),
      List(IOEx(List(1), 2), IOEx(List(2), 3))))
  }
}
