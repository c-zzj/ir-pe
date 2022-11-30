package gen.llvm

import gen.llvm.LLType._

trait Identifier extends LLVMItem

case class GlobalIdentifier(name: String) extends Identifier:
  override def toLL: String = '@' + name

case class LocalIdentifier(name: String) extends Identifier:
  override def toLL: String = '%' + name

trait Constant extends Identifier:
  def getType: LLType

case class BoolConstant(value: Boolean) extends Constant:
  override def toLL: String = if value then "true" else "false"
  override def getType: LLType = TInt(1)

case class IntConstant(numBits: Int, value: Int) extends Constant:
  override def toLL: String = value.toString
  override def getType: LLType = TInt(numBits)

case class ChrConstant(c: Char) extends Constant:
  override def toLL: String = c.toInt.toString
  override def getType: LLType = TInt(8)

case object NullPtrConstant extends Constant:
  override def toLL: String = "null"
  override def getType: LLType = TPtr(TVoid)

case class ArrayConstant(elements: List[Constant]) extends Constant:
  if (elements.isEmpty) throw new IllegalArgumentException("Array cannot have size 0")
  if (elements.exists(_ != elements.head)) throw new IllegalArgumentException("Inconsistent array element types")
  override def toLL: String = "[ " + Util.join(", ", elements.map(c => c.getType.toLL + " " + c.toLL)) + " ]"
  override def getType: LLType = TArray(elements.size, elements(1).getType)

case class StructConstant(elements: List[Constant]) extends Constant:
  override def toLL: String = "{ " + Util.join(", ", elements.map(c => c.getType.toLL + " " + c.toLL)) + " }"

  override def getType: LLType = TStruct(elements.map(e => e.getType))

case class StringConstant(s: String) extends Constant:
  override def toLL: String = "c\"" + s + "\\00\""
  override def getType: LLType = TArray(s.length + 1, TInt(8))

case class ZeroConstant(valType: LLType) extends Constant:
  override def toLL: String = "zeroinitializer"
  override def getType: LLType = valType

case object VoidConstant extends Constant:
  override def toLL: String = "void"

  override def getType: LLType = TVoid