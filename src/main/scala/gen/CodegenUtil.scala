package gen

import ir._
import gen.llvm.{ArrayConstant, Constant, IntConstant, LLType, StringConstant, StructConstant}

object CodegenUtil {
  final case class ShouldNotReach(private val message: String = "",
                                  private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

  def convertConstant(e: Exp): Constant =
    e match
      case e: IntLiteral => IntConstant(e.numBits, e.int)
      case e: StrLiteral => StringConstant(e.str)
      case e: StructArrLiteral =>
        if e.isArr then
          ArrayConstant(e.elements.map(convertConstant))
        else
          StructConstant(e.elements.map(convertConstant))
      case _ => throw ShouldNotReach()

  def convertType(irT: IRType): LLType =
    irT match
      case IRInt(i) => LLType.TInt(i)
      case IRFunction(retType, argTypeList) => LLType.TFunction(convertType(retType), argTypeList.map(convertType))
      case IRArray(elmType, size) => LLType.TOpaquePtr
      case IRStruct(elmTypeList) => LLType.TOpaquePtr
      case IRVoid => LLType.TVoid

  def convertContentType(irT: IRType): LLType =
    irT match
      case IRInt(i) => LLType.TInt(i)
      case IRFunction(retType, argTypeList) => LLType.TFunction(convertType(retType), argTypeList.map(convertType))
      case IRArray(elmType, size) => size match
        case Some(i) => LLType.TArray(i, convertType(elmType))
        case None => throw ShouldNotReach()
      case IRStruct(elmTypeList) => LLType.TStruct(elmTypeList.map(convertType))
      case IRVoid => LLType.TVoid
}
