package pe

import ast.*

import scala.collection.mutable

class DeadCodeEliminator:
  // post order traversal
  val ctx: mutable.HashSet[String] =  mutable.HashSet.empty[String]

  def elim(e: Exp): Exp =
    e match
      case BinOp(lhs, op, rhs) => BinOp(elim(lhs), op, elim(rhs))

      case StrLiteral(_) => e
      case IntLiteral(_) => e
      case Var(name) => e

      case If(cond, bThen, bElse) => If(elim(cond), elim(bThen), elim(bElse))

      case Fn(args, body, restricted, simplified, hasSideEffect) => e

      case Rec(name, Fn(args, body, r, s, h)) => e

      case Apply(fn, args) => null

      case Let(name, e) => null

      case ExpList(exps) => null

      case Arr(_) => e

      case Build(fn, size) => Build(elim(fn), elim(size))

      case ReadArr(array, index) => ReadArr(elim(array), elim(index))

      case unit => unit
