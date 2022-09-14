package ast

import scala.annotation.targetName

trait Exp

enum Op extends ASTNode:
  case ADD, SUB, MUL, DIV, MOD, GT, LT, GE, LE, NE, EQ, OR, AND

case class BinOp(lhs: Exp, op: Op, rhs: Exp) extends Exp

class Literal extends Exp

case class StrLiteral(str: String) extends Exp

case class IntLiteral(int: Int) extends Literal

case class ChrLiteral(char: Char) extends Literal

case class Var(name: String) extends Exp

case class If(cond: Exp, bThen: Exp, bElse: Exp) extends Exp

case class Fn(args: List[String], body: Exp) extends Exp

case class Rec(name: String, f: Fn) extends Exp

case class Apply(f: Fn, args: List[Exp]) extends Exp

case class ExpList(exps: List[Exp]) extends Exp

case class Let(name: String, e: Exp) extends Exp

case class Assign(lhs: Exp, rhs: Exp) extends Exp

case class Array(length: Int, itemSize: Int) extends Exp

case class ArrayAccess(array: Exp, index: Exp)

//TODO
// case class RaggedArray(slots: List[Int]) extends Exp
// MemRef ?
// MemDeref ?

case object unit extends Exp