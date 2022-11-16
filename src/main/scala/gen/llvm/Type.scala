package gen.llvm

trait Type extends LLVMItem

trait AggregateType extends Type

object Type {
  case class TInt(numBits: Int) extends Type:
    override def toLL: String = 'i' + numBits.toString

  case object TVoid extends Type:
    override def toLL: String = "void"

  case class TFunction(retType: Type, argTypeList: List[Type]) extends Type:
    override def toLL: String = retType.toString + " " + argTypeList.toString.substring(4)

  case class TPtr(contentType: Type) extends Type:
    override def toLL: String = contentType.toString + "*"

  case object TOpaquePtr extends Type:
    override def toLL: String = "ptr"

  case class TArray(size: Int, elmType: Type) extends AggregateType:
    override def toLL: String = "[" + size.toString + " x " + elmType.toLL + "]"

  case class TStruct(elmTypeList: List[Type]) extends AggregateType:
    override def toLL: String = "{ " + String.join(", ", elmTypeList.map(t => t.toLL))  +" }"
}
