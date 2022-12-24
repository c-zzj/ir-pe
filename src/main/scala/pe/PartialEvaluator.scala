package pe

import ir.*
import scala.collection.mutable

class PartialEvaluator {
  def intToBool(int: Int): Boolean = if int == 0 then false else true

  def boolToInt(bool: Boolean): Int = if bool then 1 else 0


  def isConst(e: Exp): Boolean =
    e match
      case e: BinOp => false
      case e: StrLiteral => true
      case e: IntLiteral => true
      case e: Var => false
      case e: Fn => true
      case e: Rec => true
      case e: Apply => true

  def eval(e: Stmt): Exp =
    e match
      case e: BinOp =>
        (eval(e.lhs), eval(e.rhs)) match {
          case (lhs: IntLiteral, rhs: IntLiteral) =>
            val l: Int = lhs.int
            val r: Int = rhs.int
            e.op match {
              case Op.ADD => IntLiteral(l + r)
              case Op.SUB => IntLiteral(l - r)
              case Op.MUL => IntLiteral(l * r)
              case Op.DIV => IntLiteral(l / r)
              case Op.MOD => IntLiteral(l % r)
              case Op.GT => IntLiteral(boolToInt(l >= r))
              case Op.LT => IntLiteral(boolToInt(l <= r))
              case Op.GE => IntLiteral(boolToInt(l > r))
              case Op.LE => IntLiteral(boolToInt(l < r))
              case Op.NE => IntLiteral(boolToInt(l != r))
              case Op.EQ => IntLiteral(boolToInt(l == r))
              case Op.OR => IntLiteral(boolToInt(intToBool(l) || intToBool(r)))
              case Op.AND => IntLiteral(boolToInt(intToBool(l) && intToBool(r)))
            }
          case (lhs, rhs) => BinOp(lhs,e.op, rhs)
        }
      case e: (StrLiteral | IntLiteral | Fn | Rec) => e
      case e: Var => e
      case e: Apply =>
        eval(e.fn) match {
          case f: Fn => e
        }
      case e: InitArr => e
      case e: StructArrLiteral => e
      case e: GetElementAt => e
      case e: InitClosure => e
      case e: Phi => e
      case VoidE => VoidE


  val varExpMap = mutable.HashMap.empty[String, Exp]
  val varConstMap = mutable.HashMap.empty[String, Boolean]

}
