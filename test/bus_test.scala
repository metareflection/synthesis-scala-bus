package bus

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

import ast._
import repl._
import bottomup_lib._

class BusTesting extends AnyFunSuite {
  def prettyopt(ov: Option[Value]): String = ov match {
    case None => "<none>"
    case Some(v) => pretty(v)
  }

  def readFile(filename: String): String = { 
    return Source.fromFile(filename).getLines.mkString("\n")
  }

  case class Test(name: String, formals: List[String], ios: List[IOEx], expected: Value)
  def toValueList(v: Value): List[Value] = (v: @unchecked) match {
    case N => Nil
    case P(x, rest) => x :: toValueList(rest)
  }
  def parseFormals(v: Value): List[String] = (v: @unchecked) match {
    case N => Nil
    case P(S(x), rest) => x :: parseFormals(rest)
  }
  def parseIOExs(v: Value): List[IOEx] = (v: @unchecked) match {
    case N => Nil
    case P(P(P(_, args), P(output, N)), rest) =>
      IOEx(toValueList(args), output) :: parseIOExs(rest)
  }
  def parseTest(v: Value): Test = (v: @unchecked) match {
    case P(S(name), P(formals, P(ios, P(expected, N)))) =>
      Test(name, parseFormals(formals), parseIOExs(ios), expected)
  }
  val stutterTest = parseTest(parse("""
(stutter
   (?rec xs)
   (((f '(2 2 3 3) '(1 2 3)) (1 1 2 2 3 3)))
   (cons (car xs) (cons (car xs) ?rec)))
"""))
  test("stutter") {
    val t = stutterTest
    val r = bottomup(t.formals, t.ios)
    assertResult(Some(t.expected))(r)
  }

  val src = readFile("in/dan-bottomup-data.scm")
  val tests = toValueList(parse(src)).map(parseTest)
  tests.zipWithIndex.foreach{ case(t, i) =>
    test(i.toString+" ("+t.name+" "+t.formals.mkString(" ")+")") {
      val r = bottomup(t.formals, t.ios)
      assert(Some(t.expected) == r, "\n"+pretty(t.expected) + " vs " + prettyopt(r))
    }
  }
}
