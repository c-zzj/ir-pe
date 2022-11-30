package conversion

import scala.collection.mutable

/**
 * "main" function required. Only constants and functions are allowed in global assignments
 * @param code
 * @param varCounter
 */
class IR(var code: Block, var varCounter: Int)

class Stmt

class While(var cond: Exp, var body: Block, var tail: Block) extends Stmt

class Continue extends Stmt

class Break extends Stmt

class Assign(var name: String, var value: Exp) extends Stmt

class Block(var stmts: LinkedSet[Stmt]) extends Stmt:
  def this(stmts: Stmt*) =
    this(LinkedSet[Stmt](stmts.toList))

  def this(stmts: List[Stmt]) =
    this(LinkedSet[Stmt](stmts))

class If(var cond: Exp, var bThen: Block, var bElse: Block) extends Stmt

class Return(var value: Exp) extends Stmt

trait Exp extends Stmt:
  def eType: IRType

enum Op:
  case ADD, SUB, MUL, DIV, MOD, GT, LT, GE, LE, NE, EQ, OR, AND

class BinOp(var lhs: Exp, val op: Op, var rhs: Exp, var tp: IRType = Undefined) extends Exp:
  override def eType: IRType =
    op match
      case Op.GT | Op.LT | Op.GE | Op.LE | Op.EQ | Op.NE => IRInt(1)
      case _ => tp

class StrLiteral(val str: String) extends Exp:
  override def eType: IRType = IRArray(IRInt(8), Some(str.length + 1))

class IntLiteral(val int: Int, val numBits: Int = 32) extends Exp:
  override def eType: IRType = IRInt(numBits)

class Var(var name: String, var tp: IRType = Undefined) extends Exp:
  override def eType: IRType = tp

case class NameTypePair(name: String, tp: IRType)

class Fn(var params: List[String],
         val body: Block,
         val isRestricted: Boolean = false,
         val isSimplified: Boolean = false,
         val hasSideEffect: Boolean = false,
         var env: List[NameTypePair] = List.empty[NameTypePair],
         var tp: IRType = Undefined) extends Exp:
  override def eType: IRType = tp

  def paramNameTypePairs: List[NameTypePair] =
    eType match
      case t: IRFunction =>
        (params zip t.argTypeList).map((name, tp) => NameTypePair(name, tp))
      case _ => List()


class Rec(var name: String, var fn: Fn, var tp: IRType = Undefined) extends Exp:
  override def eType: IRType = tp

class Apply(var fn: Exp, var args: List[Exp], var isClosure: Boolean = false) extends Exp:
  override def eType: IRType = fn.eType match
    case t: IRFunction => if ! isClosure then t.retType else Undefined
    case t: IRClosure => if isClosure then t.retType else Undefined
    case _ => Undefined

class InitArr(var size: Exp, var elmType: IRType) extends Exp:
  override def eType: IRType = IRArray(elmType)

class InitStruct(var elmTypes: List[IRType]) extends Exp:
  override def eType: IRType = IRStruct(elmTypes)

class StructArrLiteral(var elements: List[Exp], var isArr: Boolean = false) extends Exp:
  override def eType: IRType =
    if isArr then IRArray(elmType = elements.head.eType, size = Some(elements.size))
    else IRStruct(elements.map(e => e.eType))

class GetElementAt(var array: Exp, var index: Exp) extends Exp:
  override def eType: IRType = Undefined

class SetElementAt(var array: Exp, var index: Exp, var elm: Exp) extends Exp:
  override def eType: IRType = IRVoid

class InitClosure(var env: List[NameTypePair], var fn: NameTypePair) extends Exp:
  override def eType: IRType = fn.tp match
    case t: IRFunction => IRClosure(t.retType, t.argTypeList)
    case t: IRClosure => IRClosure(t.retType, t.argTypeList)
    case _ => Undefined

class Phi(var from: mutable.Map[Block, String] = mutable.HashMap.empty[Block, String], var tp: IRType = Undefined) extends Exp:
  override def eType: IRType = tp

case object VoidE extends Exp:
  override def eType: IRType = IRVoid


trait IRType

case object Undefined extends IRType

case class IRInt(numBits: Int) extends IRType

case object IRVoid extends IRType

case class IRFunction(retType: IRType, argTypeList: List[IRType]) extends IRType

case class IRClosure(retType: IRType, argTypeList: List[IRType]) extends IRType

case class IRArray(elmType: IRType, size: Option[Int] = None) extends IRType

case class IRStruct(elmTypeList: List[IRType]) extends IRType