package bus

object ast {
  trait Value
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
  def evalExpr(expr: Value, env: Env): Value = expr match {
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

object bottomup_lib {
  case class Piece(expr: Value, size: Int, deno: List[Value])
  abstract class Op {
    val name: String
    val arity: Int
    def applicable(v: Value): Boolean = true
    def computeExpr(e: Value): Value = P(S(name), e)
    def computeVal(v: Value): Value
  }
  val ops: List[Op] = List(
    new Op {
      override val name = "cons"
      override val arity = 2
      override def computeVal(v: Value) = (v: @unchecked) match {
        case P(a, P(d, N)) => P(a, d)
      }
    },
    new Op {
      override val name = "car"
      override val arity = 1
      override def applicable(v: Value): Boolean = v match {
        case P(P(a, d), N) => true
        case _ => false
      }
      override def computeVal(v: Value) = (v: @unchecked) match {
        case P(P(a, d), N) => a
      }
    },
    new Op {
      override val name = "cdr"
      override val arity = 1
      override def applicable(v: Value): Boolean = v match {
        case P(P(a, d), N) => true
        case _ => false
      }
      override def computeVal(v: Value) = (v: @unchecked) match {
        case P(P(a, d), N) => d
      }
    },
    new Op {
      override val name = "not"
      override val arity = 1
      override def applicable(v: Value): Boolean = v match {
        case P(B(b), N) => true
        case _ => false
      }
      override def computeVal(v: Value) = (v: @unchecked) match {
        case P(B(b), N) => B(!b)
      }
    },
    new Op {
      override val name = "add1"
      override val arity = 1
      override def applicable(v: Value): Boolean = v match {
        case P(I(n), N) => true
        case _ => false
      }
      override def computeVal(v: Value) = (v: @unchecked) match {
        case P(I(n), N) => I(n+1)
      }
    },
    new Op {
      override val name = "+"
      override val arity = 2
      override def applicable(v: Value): Boolean = v match {
        case P(I(n1), P(I(n2), N)) => true
        case _ => false
      }
      override def computeVal(v: Value) = (v: @unchecked) match {
        case P(I(n1), P(I(n2), N)) => I(n1+n2)
      }
    },
    new Op {
      override val name = "*"
      override val arity = 2
      override def applicable(v: Value): Boolean = v match {
        case P(I(n1), P(I(n2), N)) => true
        case _ => false
      }
      override def computeVal(v: Value) = (v: @unchecked) match {
        case P(I(n1), P(I(n2), N)) => I(n1*n2)
      }
    },
    new Op {
      override val name = "append"
      override val arity = 2
      override def applicable(v: Value): Boolean = v match {
        case P(xs, P(ys, N)) => isList(xs)
        case _ => false
      }
      override def computeVal(v: Value) = (v: @unchecked) match {
        case P(xs, P(ys, N)) => append(xs, ys)
      }
    },
    new Op {
      override val name = "apply1"
      override val arity = 2
      override def applicable(v: Value): Boolean = v match {
        case P(fun, P(_, N)) => fun match {
          case Prim(_, _) => true
          case Clo(_, _, _) => true
          case _ => false
        }
        case _ => false
      }
      override def computeExpr(e: Value): Value = e
      override def computeVal(v: Value) = (v: @unchecked) match {
        case P(fun, args) => applyFun(fun, args)
      }
    }
  )
  case class IOEx(args: List[Value], output: Value)
  def bottomup(formals: List[String], ios: List[IOEx], maxSize: Int = 8): Option[Value] = {
    val n = formals.length
    val inputs = ios.map{io => io.args.map(eval)}
    val outputs = ios.map{io => io.output}
    val inputPieces = (for (i <- 0 until n) yield Piece(S(formals(i)), 1, inputs.map(_(i)))).toList
    val constantPieces = Nil // TODO
    bottomupIter(outputs, 1, List(Nil, inputPieces ++ constantPieces), maxSize)
  }

  def toOption[A](xs: List[A]): Option[A] = xs match {
    case Nil => None
    case x::_ => Some(x)
  }

  def getDenotPiece(deno: List[Value], pieces: List[Piece]): Option[Piece] =
    toOption(pieces.filter(_.deno == deno))

  def toListValue(lst: List[Value]): Value = lst match {
    case Nil => N
    case v::rest => P(v, toListValue(rest))
  }
  def piecesArguments(ps: List[Piece]): List[Value] =
    ps.map(_.deno).transpose.map(toListValue)
  def applicableOp(op: Op, ps: List[Piece]): Boolean =
    piecesArguments(ps).forall(op.applicable)
  def applyOp(op: Op, ps: List[Piece]): Option[Piece] = {
    var odeno: Option[List[Value]] = None
    try {
      odeno = Some(piecesArguments(ps).map(op.computeVal))
    } catch {
      case _ =>
    }
    odeno.map{deno =>
      val expr = op.computeExpr(toListValue(ps.map(_.expr)))
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

  def bottomupIter(outputs: List[Value], size: Int, piecess: List[List[Piece]], maxSize: Int): Option[Value] = getDenotPiece(outputs, piecess.last) match {
    case Some(piece) => Some(piece.expr)
    case None => {
      val newSize = size + 1
      if (newSize < maxSize) {
        val newPieces = computeNewPieces(newSize, piecess)
        val uniqueNewPieces = withoutDenos(piecess, uniqueDenos(newPieces))
        val newPiecess = piecess ++ List(uniqueNewPieces)
        bottomupIter(outputs, newSize, newPiecess, maxSize)
      } else None
    }
  }
}
