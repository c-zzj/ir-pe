package gen.llvm

import gen.llvm.CmpCode
import gen.llvm.CmpCode.CmpCode
import gen.llvm.OpCode.*
import gen.llvm.Type.TVoid

trait Instruction extends LLVMItem

object Instruction {
  trait Terminator extends Instruction

  case class Return(retType: Type, retValue: Identifier) extends Terminator :
    def this() = this(TVoid, LocalIdentifier("placeholder"))
    override def toLL: String =
      if retType == TVoid then RET.asmCode + TVoid.toLL
      else String.join(" ", RET.asmCode, retType.toLL, retValue.toLL)

  case class Branch(dest: Identifier) extends Terminator :
    override def toLL: String = String.join(" ", BR.asmCode, "label", dest.toLL)

  case class CondBranch(cond: Identifier, ifTrue: Identifier, ifFalse: Identifier) extends Terminator :
    override def toLL: String = String.join(" ", BR.asmCode, "i1", cond.toLL + ",", "label", ifTrue.toLL + ",", "label", ifFalse.toLL)

  case class BinaryInstruction(res: Identifier, opCode: OpCode.BinOp, opType: Type, op1: Identifier, op2: Identifier) extends Instruction:
    override def toLL: String = String.join(" ", res.toLL, "=", opCode.asmCode, opType.toLL, op1.toLL + ',', op2.toLL)

  case class StackAlloc(res: Identifier, elmType: Type, sizeType: Type, size: Identifier) extends Instruction:
    override def toLL: String = String.join(" ", res.toLL, "=", ALLOCA.asmCode, elmType.toLL, sizeType.toLL, size.toLL)

  case class ExtractValue(res: Identifier, aggType: AggregateType, aggregate: Identifier, index: Identifier) extends Instruction:
    override def toLL: String =
      String.join(" ", res.toLL, "=", EXTRACTVALUE.asmCode, aggregate.toLL + ',', index.toLL)

  case class InsertValue(aggType: AggregateType, aggregate: Identifier, elmType: Type, elm: Identifier, index: Identifier) extends Instruction:
    override def toLL: String =
      String.join(" ", INSERTVALUE.asmCode, aggType.toLL, aggregate.toLL + ',', elmType.toLL, elm.toLL + ',', index.toLL)

  case class Load(res: Identifier, valType: Type, pointer: Identifier) extends Instruction:
    override def toLL: String = String.join(" ", res.toLL, "=", LOAD.asmCode, valType.toLL + ',', "ptr", pointer.toLL)

  case class Store(valType: Type, value: Identifier, pointer: Identifier) extends Instruction:
    override def toLL: String = String.join(" ", STORE.asmCode, valType.toLL, value.toLL + ',', "ptr", pointer.toLL)

  case class GetElementPtr(res: Identifier, baseType: Type, pointer: Identifier, indices: List[(Type, Identifier)]) extends Instruction:
    override def toLL: String = String.join(" ", res.toLL, GETELEMENTPTR.asmCode, baseType.toLL + ",",
    "ptr", pointer.toLL + ",", Util.join(", ", indices.map((t, i) => t.toLL + i.toLL))
    )

  case class Cast(res: Identifier, opCode: CastOp, typeFrom: Type, valueFrom: Identifier, typeTo: Type) extends Instruction:
    override def toLL: String = String.join(" ", res.toLL, "=", opCode.asmCode, typeFrom.toLL, valueFrom.toLL, "to", typeTo.toLL)

  case class Compare(res: Identifier, cond: CmpCode, valType: Type, op1: Identifier, op2: Identifier) extends Instruction:
    override def toLL: String = String.join(" ", res.toLL, "=", ICMP.asmCode, cond.toString, valType.toLL, op1.toLL, op2.toLL)

  case class Phi(res: Identifier, valType: Type, valLabelPairs: List[(Identifier, Identifier)]) extends Instruction:
    override def toLL: String =
      val valLabelString = Util.join(", ", valLabelPairs.map((value, label) => "[ " + value.toLL + ", " + label.toLL + " ]"))
      String.join(" ", res.toLL, "=", PHI.asmCode, valType.toLL, valLabelString)

  case class FunCall(res: Identifier, valType: Type, funVal: Identifier, funArgs: List[(Type, Identifier)]) extends Instruction:
    override def toLL: String =
      val argString = Util.join(", ", funArgs.map((argType, argVal) => argType.toLL + " " + argVal.toLL))
      String.join(" ", res.toLL, "=", CALL.asmCode, valType.toLL, funVal.toLL + "(" + argString + ")")

  /**
   * Calculate the size of a LLVM type. The result has type i32.
   * @param res identifier of the result
   * @param tp type to calculate
   * @param tmpIdentifier temporary identifier with unique name
   */
  case class TypeOf(res: Identifier, tp: Type, tmpIdentifier: Identifier) extends Instruction:
    /*
     * example:
    %Size = getelementptr %T* null, i32 1
    %SizeI = ptrtoint %T* %Size to i32
     */
    override def toLL: String =
      s"""${tmpIdentifier.toLL} = getelementptr ${tp.toLL}, ptr null, i32 1
         |\t${res.toLL} = ptrtoint ptr ${tmpIdentifier.toLL} to i32
         |""".stripMargin

  case class OffsetOf(res: Identifier, tp: Type, index: Int, tmpIdentifier: Identifier) extends Instruction:
    override def toLL: String =
      s"""${tmpIdentifier.toLL} = getelementptr ${tp.toLL}, ptr null, i32 0, i32 $index
         |\t${res.toLL} = ptrtoint ptr ${tmpIdentifier.toLL} to i32
         |""".stripMargin
}

