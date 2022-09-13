package pe
import ast.*

import scala.collection.mutable

object ConstProp:
  def visit(e: Exp): Exp =
    val nameValMap = mutable.HashMap.empty[String, Exp]

    e match
      case BinOp(lhs, op, rhs) => BinOp(visit(lhs), op, visit(rhs))
      case Var(name) => nameValMap.get(name) match
        case None => Var(name)
        case Some(StrLiteral(str)) => StrLiteral(str)
        case Some(IntLiteral(int)) => IntLiteral(int)
        case Some(ChrLiteral(char)) => ChrLiteral(char)
        case Some(other) => other
      case other => other
