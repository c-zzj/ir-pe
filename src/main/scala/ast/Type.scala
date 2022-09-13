package ast

trait Type extends ASTNode

enum BaseType extends Type:
  case INT, CHAR, VOID

case class ArrayType(T: Type, size: Int) extends Type

case class FunType(argTypes: List[Type], retType: Type) extends Type