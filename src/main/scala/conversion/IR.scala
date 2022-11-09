package conversion

import scala.collection.mutable

class IR(var prog: Block, var varCounter: Int)

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


trait Exp extends Stmt

enum Op:
  case ADD, SUB, MUL, DIV, MOD, GT, LT, GE, LE, NE, EQ, OR, AND

class BinOp(var lhs: Exp, val op: Op, var rhs: Exp) extends Exp

class ChrLiteral(val char: Char) extends Exp

class IntLiteral(val int: Int) extends Exp

class Var(var name: String) extends Exp

class Fn(var params: List[String],
         val body: Block,
         val isRestricted: Boolean = false,
         val isSimplified: Boolean = false,
         val hasSideEffect: Boolean = false,
         var env: List[String] = List.empty[String]) extends Exp

class Rec(var name: String, var fn: Fn) extends Exp

class Apply(var fn: Exp, var args: List[Exp]) extends Exp

class Build(var fn: Exp, var size: Exp) extends Exp

class Arr(var elements: List[Exp]) extends Exp

class ReadArr(var array: Exp, var index: Exp) extends Exp

class InitClosure(var env: List[String], var fn: String) extends Exp

class Phi(var from: mutable.Map[Block, String] = mutable.HashMap.empty[Block, String]) extends Exp

case object UnitE extends Exp