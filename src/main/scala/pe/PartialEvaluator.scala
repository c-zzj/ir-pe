package pe

import ast.*

import scala.collection.mutable

class PartialEvaluator:
  val nameValMap = mutable.HashMap.empty[String, Exp]

  def visit(e: Exp): Exp =
    e match
      case BinOp(lhs, op, rhs) => {
        val l = visit(lhs)
        val r = visit(rhs)
        if ! (l.isInstanceOf[Literal] || r.isInstanceOf[Literal])
          then BinOp(l, op, r)
        else
          op match
            case Op.ADD => null
            case Op.SUB => null
            case Op.MUL => null
            case Op.DIV => null
            case Op.MOD => null
            case Op.GT => null
            case Op.LT => null
            case Op.GE => null
            case Op.LE => null
            case Op.NE => null
            case Op.EQ => null
            case Op.OR => null
            case Op.AND => null
      }
      case StrLiteral(str) => StrLiteral(str)
      case IntLiteral(int) => IntLiteral(int)
      case ChrLiteral(char) => ChrLiteral(char)
      case Var(name) =>
        val value = nameValMap.get(name)
        
      case If(cond, bIf, bElse) => null
      case Fn(args, body) => null
      case Rec(name, f) => null
      case Apply(f, args) => null
      case ExpList(exps) => null
      case Let(name, e) => null


