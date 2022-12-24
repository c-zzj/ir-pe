package gen.llvm

import gen.llvm.CmpCode
import gen.llvm.CmpCode.CmpCode
import gen.llvm.OpCode.*
import gen.llvm.LLType.TVoid

trait Instruction extends LLVMItem

object Instruction {
  trait Terminator extends Instruction

  case class Return(retType: LLType, retValue: Identifier) extends Terminator :
    def this() = this(TVoid, LocalIdentifier("placeholder"))
    override def toLL: String =
      if retType == TVoid then RET.asmCode + " " + TVoid.toLL
      else String.join(" ", RET.asmCode, retType.toLL, retValue.toLL)

  case class Branch(dest: Label) extends Terminator :
    override def toLL: String = String.join(" ", BR.asmCode, "label", dest.toIdentifier.toLL)

  case class CondBranch(cond: Identifier, ifTrue: Label, ifFalse: Label) extends Terminator :
    override def toLL: String =
      String.join(" ", BR.asmCode, "i1", cond.toLL + ",", "label", ifTrue.toIdentifier.toLL + ",", "label", ifFalse.toIdentifier.toLL)

  case class BinaryInstruction(res: LocalIdentifier, opCode: OpCode.BinOpCode, opType: LLType, op1: Identifier, op2: Identifier) extends Instruction:
    override def toLL: String = String.join(" ", res.toLL, "=", opCode.asmCode, opType.toLL, op1.toLL + ',', op2.toLL)

  case class StackAlloc(res: LocalIdentifier, elmType: LLType, sizeType: LLType, size: Identifier) extends Instruction:
    override def toLL: String = String.join(" ", res.toLL, "=", ALLOCA.asmCode, elmType.toLL, sizeType.toLL, size.toLL)

  case class ExtractValue(res: LocalIdentifier, aggType: AggregateType, aggregate: Identifier, index: Identifier) extends Instruction:
    override def toLL: String =
      String.join(" ", res.toLL, "=", EXTRACTVALUE.asmCode, aggregate.toLL + ',', index.toLL)

  case class InsertValue(aggType: AggregateType, aggregate: Identifier, elmType: LLType, elm: Identifier, index: Identifier) extends Instruction:
    override def toLL: String =
      String.join(" ", INSERTVALUE.asmCode, aggType.toLL, aggregate.toLL + ',', elmType.toLL, elm.toLL + ',', index.toLL)

  case class Load(res: LocalIdentifier, valType: LLType, pointer: Identifier) extends Instruction:
    override def toLL: String = String.join(" ", res.toLL, "=", LOAD.asmCode, valType.toLL + ',', "ptr", pointer.toLL)

  case class Store(valType: LLType, value: Identifier, pointer: Identifier) extends Instruction:
    override def toLL: String = String.join(" ", STORE.asmCode, valType.toLL, value.toLL + ',', "ptr", pointer.toLL)

  case class GetElementPtr(res: LocalIdentifier, baseType: LLType, pointer: Identifier, indices: List[(LLType, Identifier)]) extends Instruction:
    override def toLL: String = String.join(" ", res.toLL, GETELEMENTPTR.asmCode, baseType.toLL + ",",
    "ptr", pointer.toLL + ",", Util.join(", ", indices.map((t, i) => t.toLL + i.toLL))
    )

  case class Cast(res: LocalIdentifier, opCode: CastOp, typeFrom: LLType, valueFrom: Identifier, typeTo: LLType) extends Instruction:
    override def toLL: String = String.join(" ", res.toLL, "=", opCode.asmCode, typeFrom.toLL, valueFrom.toLL, "to", typeTo.toLL)

  case class Compare(res: LocalIdentifier, cond: CmpCode, valType: LLType, op1: Identifier, op2: Identifier) extends Instruction:
    override def toLL: String = String.join(" ", res.toLL, "=", ICMP.asmCode, cond.toString, valType.toLL, op1.toLL + ", " + op2.toLL)

  case class Phi(res: LocalIdentifier, valType: LLType, valLabelPairs: List[(Identifier, Label)]) extends Instruction:
    override def toLL: String =
      val valLabelString = Util.join(", ",
        valLabelPairs.map((value: Identifier, label: Label) => "[ " + value.toLL + ", " + label.toIdentifier.toLL + " ]")
      )
      String.join(" ", res.toLL, "=", PHI.asmCode, valType.toLL, valLabelString)

  case class FunCall(res: Option[LocalIdentifier], valType: LLType, funVal: Identifier, funArgs: List[(LLType, Identifier)]) extends Instruction:
    override def toLL: String =
      val argString = Util.join(", ", funArgs.map((argType, argVal) => argType.toLL + " " + argVal.toLL))
      if valType != TVoid then
        res match
          case Some(id) =>
            String.join(" ", id.toLL, "=", CALL.asmCode, valType.toLL, funVal.toLL + "(" + argString + ")")
          case None => String.join(" ", CALL.asmCode, valType.toLL, funVal.toLL + "(" + argString + ")")
      else
        String.join(" ", CALL.asmCode, valType.toLL, funVal.toLL + "(" + argString + ")")

  case object Unreachable extends Instruction:
    override def toLL: String = "unreachable"

  /**
   * Calculate the size of a LLVM type. The result has type i32.
   * @param res identifier of the result
   * @param tp type to calculate
   * @param tmpIdentifier temporary identifier with unique name
   */
  case class SizeOf(res: LocalIdentifier, tp: LLType, tmpIdentifier: LocalIdentifier) extends Instruction:
    /*
     * example:
    %Size = getelementptr %T* null, i32 1
    %SizeI = ptrtoint %T* %Size to i32
     */
    override def toLL: String =
      s"""${tmpIdentifier.toLL} = getelementptr ${tp.toLL}, ptr null, i32 1
         |\t${res.toLL} = ptrtoint ptr ${tmpIdentifier.toLL} to i32
         |""".stripMargin

  case class OffsetOf(res: LocalIdentifier, tp: LLType, index: Int, tmpIdentifier: LocalIdentifier) extends Instruction:
    override def toLL: String =
      s"""${tmpIdentifier.toLL} = getelementptr ${tp.toLL}, ptr null, i32 0, i32 $index
         |\t${res.toLL} = ptrtoint ptr ${tmpIdentifier.toLL} to i32
         |""".stripMargin
}

