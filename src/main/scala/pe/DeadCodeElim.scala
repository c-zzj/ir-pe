package pe

import ast.*

object DeadCodeElim:
  def visit(e: Exp): Exp =
    e match
      case BinOp(lhs, op, rhs) => null
      case StrLiteral(str) => null
      case IntLiteral(int) => null
      case ChrLiteral(char) => null
      case Var(name) => null
      case If(cond, bIf, bElse) => null
      case Fn(args, body) => null
      case Rec(name, f) => null
      case Apply(f, args) => null
      case ExpList(exps) => null
      case Let(name, e) => null

case class ExpWrapper(e: Exp, hasSideEffect: Boolean = false, refCount: Int = 0)

