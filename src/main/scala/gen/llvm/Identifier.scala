package gen.llvm

import gen.llvm.Type._

trait Identifier extends LLVMItem

case class GlobalIdentifier(name: String) extends Identifier:
  override def toLL: String = '@' + name

case class LocalIdentifier(name: String) extends Identifier:
  override def toLL: String = '%' + name

trait Constant extends Identifier:
  def getType: Type

case class BoolConstant(value: Boolean) extends Constant:
  override def toLL: String = if value then "true" else "false"
  override def getType: Type = TInt(1)

case class IntConstant(numBits: Int, value: Int) extends Constant:
  override def toLL: String = value.toString
  override def getType: Type = TInt(numBits)

case class ChrConstant(c: Char) extends Constant:
  override def toLL: String = c.toInt.toString
  override def getType: Type = TInt(8)

case object NullPtrConstant extends Constant:
  override def toLL: String = "null"
  override def getType: Type = TPtr(TVoid)

case class ArrayConstant(elements: List[Constant]) extends Constant:
  if (elements.isEmpty) throw new IllegalArgumentException("Array cannot have size 0")
  override def toLL: String = "[ " + Util.join(", ", elements.map(c => c.getType.toLL + " " + c.toLL)) + " ]"
  override def getType: Type = TArray(elements.size, elements(1).getType)

case class StringConstant(s: String) extends Constant:
  override def toLL: String = "c\"" + s + "\\00\""
  override def getType: Type = TArray(s.length + 1, TInt(8))

case class ZeroConstant(valType: Type) extends Constant:
  override def toLL: String = "zeroinitializer"
  override def getType: Type = valType