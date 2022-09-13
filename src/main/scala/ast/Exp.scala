package ast

import scala.annotation.targetName

trait Exp extends ASTNode

case class UnitExp(e: Exp) extends Exp

enum Op extends ASTNode:
  case ADD, SUB, MUL, DIV, MOD, GT, LT, GE, LE, NE, EQ, OR, AND

case class BinOp(lhs: Exp, op: Op, rhs: Exp) extends Exp

case class StrLiteral(str: String) extends Exp

case class IntLiteral(int: Int) extends Exp

case class ChrLiteral(char: Char) extends Exp

case class Var(name: String) extends Exp

case class Branch(cond: Exp, ifBranch: Exp, elseBranch: Exp) extends Exp

case class Fn(args: List[String], body: Exp) extends Exp

case class Rec(name: String, f: Fn) extends Exp

case class Apply(f: Fn, args: List[Exp]) extends Exp

case class ExpList(exps: List[Exp]) extends Exp

case class Let(name: String, e: Exp) extends Exp