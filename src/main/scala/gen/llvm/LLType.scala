package gen.llvm

trait LLType extends LLVMItem

trait AggregateType extends LLType

object LLType {
  case class TInt(numBits: Int) extends LLType:
    override def toLL: String = 'i' + numBits.toString

  case object TVoid extends LLType:
    override def toLL: String = "void"

  case class TFunction(retType: LLType, argTypeList: List[LLType]) extends LLType:
    override def toLL: String = retType.toString + " " + argTypeList.toString.substring(4)

  case class TPtr(contentType: LLType) extends LLType:
    override def toLL: String = contentType.toString + "*"

  case object TOpaquePtr extends LLType:
    override def toLL: String = "ptr"

  case class TArray(size: Int, elmType: LLType) extends AggregateType:
    override def toLL: String = "[" + size.toString + " x " + elmType.toLL + "]"

  case class TStruct(elmTypeList: List[LLType]) extends AggregateType:
    override def toLL: String = "{ " + Util.join(", ", elmTypeList.map(t => t.toLL))  +" }"
}
