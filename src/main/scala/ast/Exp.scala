package ast

import scala.annotation.targetName

trait Exp

enum Op extends ASTNode:
  case ADD, SUB, MUL, DIV, MOD, GT, LT, GE, LE, NE, EQ, OR, AND

case class BinOp(lhs: Exp, op: Op, rhs: Exp) extends Exp

class Literal extends Exp

case class StrLiteral(str: String) extends Exp

case class IntLiteral(int: Int) extends Literal

case class Var(name: String) extends Exp

case class If(cond: Exp, bThen: Exp, bElse: Exp) extends Exp
case class Fn(params: List[String], body: Exp, isRestricted: Boolean = false, isSimplified: Boolean = false, hasSideEffect: Boolean = false) extends Exp

case class Rec(name: String, fn: Fn) extends Exp

case class Apply(fn: Exp, args: List[Exp]) extends Exp

case class Let(name: String, e: Exp) extends Exp

case class ExpList(exps: List[Exp]) extends Exp

case class Build(fn: Exp, size: Exp) extends Exp

case class Arr(elements: List[Exp]) extends Exp
case class ReadArr(array: Exp, index: Exp) extends Exp
case object unit extends Exp

