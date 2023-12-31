package bus

object ast {
  sealed trait Value
  case class B(b: Boolean) extends Value                      // Boolean
  case class I(n: Int) extends Value                          // Int
  case class S(sym: String) extends Value                     // Symbol
  case object N extends Value                                 // Nil
  case class P(car: Value, cdr: Value) extends Value          // Pair
  case class Prim(name: String, impl: Value => Value) extends Value // Primitive
  type Env = Map[String,Value]
  case class Clo(body: Value, formals: Value, env: Env) extends Value // Closure
}
import ast._

import scala.util.parsing.combinator._
object parser extends JavaTokenParsers {
  def exp: Parser[Value] =
    "#f" ^^ { case _ => B(false) }
  | "#t" ^^ { case _ => B(true) }
  | wholeNumber ^^ { case s => I(s.toInt) }
  | """[^\s\(\)'"]+""".r ^^ { case s => S(s) }
  | "'" ~> exp ^^ { case s => P(S("quote"), P(s, N)) }
  | "()" ^^ { case _ => N }
  | "(" ~> exps <~ ")" ^^ { case vs => vs }

  def exps: Parser[Value] =
    exp ~ exps ^^ { case v~vs => P(v, vs) }
  | exp ^^ { case v => P(v, N) }
}
import parser._

object repl {
  def parse(s: String): Value = {
    val Success(e, _) = (parseAll(exp, s): @unchecked)
    e
  }
}
import repl._

object utils {
  def mapValueList(f: Value => Value, lst: Value): Value =
    (lst: @unchecked) match {
      case N => N
      case P(first, rest) => P(f(first), mapValueList(f, rest))
    }

  def extendEnv(formals: Value, args: Value, env: Env): Env =
    ((formals, args): @unchecked) match {
      case (S(x), _) => env + (x -> args)
      case (N, N) => env
      case (P(S(x1), xs), P(v1, vs)) => (extendEnv(xs, vs, env) + (x1 -> v1))
    }

  def isList(v: Value): Boolean = v match {
    case N => true
    case P(x, y) => isList(y)
    case _ => false
  }

  def append(xs: Value, ys: Value): Value = (xs: @unchecked) match {
    case N => ys
    case P(x, xs) => P(x, append(xs, ys))
  }
}
import utils._

object eval_lib {
  def evalExpr(expr: Value, env: Env): Value = (expr: @unchecked) match {
    case B(_) => expr
    case I(_) => expr
    case S(sym) => env(sym)
    case P(S("quote"), P(v, N)) => v
    case P(S("if"), P(cond, P(conseq, P(alt, N)))) =>
      evalExpr(cond, env) match {
        case B(b) if !b => evalExpr(alt, env)
        case _ => evalExpr(conseq, env)
      }
    case P(S("lambda"), P(params, P(body, N))) =>
      Clo(body, params, env)
    case P(fun, args) =>
      applyFun(evalExpr(fun, env), mapValueList({e => evalExpr(e, env)}, args))
  }

  def applyFun(fun: Value, args: Value): Value = (fun: @unchecked) match {
    case Prim(_, impl) => impl(args)
    case Clo(body, formals, env) =>
      evalExpr(body, extendEnv(formals, args, env))
  }

  lazy val initEnv: Env = bottomup_lib.ops.map{op =>
    (op.name, Prim(op.name, op.computeVal))}.toMap ++
  List(
    Prim("list", {v => v}),
    Prim("even?", {v => (v: @unchecked) match {
      case P(I(n), N) => B(n % 2 == 0)
    }})
  ).map{p => (p.name, p)}.toMap

  def eval(e: Value) = evalExpr(e, initEnv)
}
import eval_lib._

object bottomup_lib extends BottomUpSearch {
  type V = Value
  override def pretty(v: Value): String = v match {
    case B(b) => if (b) "#t" else "#f"
    case I(n) => n.toString
    case S(sym) => sym
    case N => "'()"
    case P(car, cdr) => "(" + pretty(car) + (cdr match {
      case N => ")"
      case P(_, _) => " " + pretty(cdr).substring(1)
      case _ => " . " + pretty(cdr) + ")"
    })
    case Prim(_, _) => "#<procedure>"
    case Clo(_, _, _) => "#<procedure>"
  }
  override def evalExpr(e: V): V = eval(e)
  override def formalExpr(s: String): V = S(s)
  def toListV(lst: List[V]): V = lst match {
    case Nil => N
    case v::rest => P(v, toListV(rest))
  }
  abstract class SchemeOp extends Op {
    def applicable(v: V): Boolean = true
    def computeExpr(e: V): V = P(S(name), e)
    def computeVal(v: V): V
    override def applicableFromList(vs: List[V]): Boolean = applicable(toListV(vs))
    override def computeExprFromList(es: List[V]): V = computeExpr(toListV(es))
    override def computeValFromList(vs: List[V]): V = computeVal(toListV(vs))
  }
  override lazy val ops: List[SchemeOp] = List(
    new SchemeOp {
      override val name = "cons"
      override val arity = 2
      override def computeVal(v: V) = (v: @unchecked) match {
        case P(a, P(d, N)) => P(a, d)
      }
    },
    new SchemeOp {
      override val name = "car"
      override val arity = 1
      override def applicable(v: V): Boolean = v match {
        case P(P(a, d), N) => true
        case _ => false
      }
      override def computeVal(v: V) = (v: @unchecked) match {
        case P(P(a, d), N) => a
      }
    },
    new SchemeOp {
      override val name = "cdr"
      override val arity = 1
      override def applicable(v: V): Boolean = v match {
        case P(P(a, d), N) => true
        case _ => false
      }
      override def computeVal(v: V) = (v: @unchecked) match {
        case P(P(a, d), N) => d
      }
    },
    new SchemeOp {
      override val name = "not"
      override val arity = 1
      override def applicable(v: V): Boolean = v match {
        case P(B(b), N) => true
        case _ => false
      }
      override def computeVal(v: V) = (v: @unchecked) match {
        case P(B(b), N) => B(!b)
      }
    },
    new SchemeOp {
      override val name = "add1"
      override val arity = 1
      override def applicable(v: V): Boolean = v match {
        case P(I(n), N) => true
        case _ => false
      }
      override def computeVal(v: V) = (v: @unchecked) match {
        case P(I(n), N) => I(n+1)
      }
    },
    new SchemeOp {
      override val name = "+"
      override val arity = 2
      override def applicable(v: V): Boolean = v match {
        case P(I(n1), P(I(n2), N)) => true
        case _ => false
      }
      override def computeVal(v: V) = (v: @unchecked) match {
        case P(I(n1), P(I(n2), N)) => I(n1+n2)
      }
    },
    new SchemeOp {
      override val name = "*"
      override val arity = 2
      override def applicable(v: V): Boolean = v match {
        case P(I(n1), P(I(n2), N)) => true
        case _ => false
      }
      override def computeVal(v: V) = (v: @unchecked) match {
        case P(I(n1), P(I(n2), N)) => I(n1*n2)
      }
    },
    new SchemeOp {
      override val name = "append"
      override val arity = 2
      override def applicable(v: V): Boolean = v match {
        case P(xs, P(ys, N)) => isList(xs)
        case _ => false
      }
      override def computeVal(v: V) = (v: @unchecked) match {
        case P(xs, P(ys, N)) => append(xs, ys)
      }
    },
    new SchemeOp {
      override val name = "apply1"
      override val arity = 2
      override def applicable(v: V): Boolean = v match {
        case P(fun, P(_, N)) => fun match {
          case Prim(_, _) => true
          case Clo(_, _, _) => true
          case _ => false
        }
        case _ => false
      }
      override def computeExpr(e: V): V = e
      override def computeVal(v: V) = (v: @unchecked) match {
        case P(fun, args) => applyFun(fun, args)
      }
    }
  )
}

trait BottomUpSearch {
  type V
  def evalExpr(e: V): V // TODO: distinguish between expressions and values
  def formalExpr(s: String): V
  abstract class Op {
    val name: String
    val arity: Int
    def applicableFromList(vs: List[V]): Boolean
    def computeExprFromList(es: List[V]): V
    def computeValFromList(vs: List[V]): V
  }
  lazy val ops: List[Op]
  def pretty(v: V): String
  case class Piece(expr: V, size: Int, deno: List[V]) {
    override def toString =
      s"  (expr = ${pretty(expr)},\n   deno = ${deno.map(pretty).mkString("[", ", ", "]")})"
  }
  case class IOEx(args: List[V], output: V)
  def extractConstants(ios: List[IOEx]): List[Piece] = Nil
  def bottomup(formals: List[String], ios: List[IOEx], maxSize: Int = 8): Option[V] = {
    val n = formals.length
    val inputs = ios.map{io => io.args.map(evalExpr)}
    val outputs = ios.map{io => io.output}
    val inputPieces = (for (i <- 0 until n) yield Piece(formalExpr(formals(i)), 1, inputs.map(_(i)))).toList
    val constantPieces = extractConstants(ios)
    val pieces1 = uniqueDenos(inputPieces ++ constantPieces)
    logIter(1, pieces1)
    bottomupIter(outputs, 1, List(Nil, pieces1), maxSize)
  }

  def toOption[A](xs: List[A]): Option[A] = xs match {
    case Nil => None
    case x::_ => Some(x)
  }

  def getDenotPiece(deno: List[V], pieces: List[Piece]): Option[Piece] =
    toOption(pieces.filter(_.deno == deno))
  def piecesArguments(ps: List[Piece]): List[List[V]] =
    ps.map(_.deno).transpose
  def applicableOp(op: Op, ps: List[Piece]): Boolean =
    piecesArguments(ps).forall(op.applicableFromList)
  def applyOp(op: Op, ps: List[Piece]): Option[Piece] = {
    val args = piecesArguments(ps)
    var odeno: Option[List[V]] = None
    try {
      odeno = Some(args.map(op.computeValFromList))
    } catch {
      case _ =>
    }
    odeno.map{deno =>
      val expr = op.computeExprFromList(ps.map(_.expr))
      val size = 1 + ps.map(_.size).sum
      Piece(expr, size, deno)
    }
  }
  def allPiecesArgs(arity: Int, size: Int, piecess: List[List[Piece]]): List[List[Piece]] =
    if (arity == 0) List(Nil)
    else if (size <= 0) Nil
    else if (arity == 1) piecess(size - 1).map(_::Nil)
    else
      (1 until size).toList.map{i =>
        val pieces = piecess(i)
        val rec = allPiecesArgs(arity-1, size-i, piecess)
        pieces.map{piece => rec.map(piece :: _)}.flatten
      }.flatten
  def computeNewPieces(newSize: Int, piecess: List[List[Piece]]): List[Piece] =
    ops.map{op => allPiecesArgs(op.arity, newSize, piecess).map{pieceArgs =>
      if (applicableOp(op, pieceArgs)) applyOp(op, pieceArgs)
      else None
    }.flatten}.flatten
  def uniqueDenos(pieces: List[Piece]): List[Piece] = pieces match {
    case Nil => Nil
    case p1::rest =>
      val rec = uniqueDenos(rest)
      if (rest.exists{p2 => p1.deno == p2.deno}) rec
      else p1 :: rec
  }
  def withoutDenos(piecess: List[List[Piece]], newPieces: List[Piece]): List[Piece] =
    newPieces.filter{p => !(piecess.exists{pieces => pieces.exists{old_p =>
      old_p.deno == p.deno
    }})}

  def bottomupIter(outputs: List[V], size: Int, piecess: List[List[Piece]], maxSize: Int): Option[V] = getDenotPiece(outputs, piecess.last) match {
    case Some(piece) => Some(piece.expr)
    case None => {
      val newSize = size + 1
      if (newSize < maxSize) {
        val newPieces = computeNewPieces(newSize, piecess)
        val uniqueNewPieces = withoutDenos(piecess, uniqueDenos(newPieces))
        logIter(newSize, uniqueNewPieces)
        val newPiecess = piecess ++ List(uniqueNewPieces)
        bottomupIter(outputs, newSize, newPiecess, maxSize)
      } else None
    }
  }

  def logIter(size: Int, pieces: List[Piece]): Unit = {
    log("=== SIZE "+size.toString)
    log(pieces.mkString("\n"))
  }

  val debug: Boolean = false
  def log(s: => String): Unit = {
    if (debug) println(s)
  }
}
