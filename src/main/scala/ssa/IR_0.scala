package ssa

import ast.*

trait IR_0

trait Stmt extends IR_0

class Let(val name: String, val value: Exp) extends Stmt

class Block(val stmts: List[Stmt]) extends Stmt

class If(val cond: Exp, val bThen: Block, val bElse: Block) extends Stmt

class Return(val value: Exp) extends Stmt


trait Exp extends Stmt

enum Op:
  case ADD, SUB, MUL, DIV, MOD, GT, LT, GE, LE, NE, EQ, OR, AND

class BinOp(val lhs: Exp, val op: Op, val rhs: Exp) extends Exp

class StrLiteral(val str: String) extends Exp

class IntLiteral(val int: Int) extends Exp

class Var(var name: String) extends Exp

class Fn(val params: List[String],
         val body: Block,
         val isRestricted: Boolean = false,
         val isSimplified: Boolean = false,
         val hasSideEffect: Boolean = false) extends Exp

class Rec(val name: String, val fn: Fn) extends Exp

class Apply(val fn: Exp, val args: List[Exp]) extends Exp

class Build(val fn: Exp, val size: Exp) extends Exp

class Arr(val elements: List[Exp]) extends Exp

class ReadArr(val array: Exp, val index: Exp) extends Exp

case object UnitE extends Exp