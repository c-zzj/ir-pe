package gen

import gen.llvm.{CmpCode, Instruction, LLType, LLVMItem, Label, LocalIdentifier, OpCode, Section}
import conversion.*

import collection.mutable

private def convertType(irT: IRType): LLType =
  irT match
    case IRInt(i) => LLType.TInt(i)
    case IRFunction(retType, argTypeList) => LLType.TFunction(convertType(retType), argTypeList.map(convertType))
    case IRArray(elmType, size) => LLType.TOpaquePtr
    case IRStruct(elmTypeList) => LLType.TStruct(elmTypeList.map(convertType))
    case IRClosure(retType, argTypeList) => LLType.TFunction(convertType(retType), argTypeList.map(convertType))
    case IRVoid => LLType.TVoid



class FunGen(val pInfo: ProgramInfo) {
  private val blockLabelMap = mutable.HashMap[Block, Label]
  def gen(section: Section, fn: Fn, fnName: String): Unit =

    def gen_(stmt: Stmt): Unit =
      stmt match
        case s: Block =>
        case s: If =>
        case s: Return =>
        case s: Exp =>
}

class ExpGen(val pInfo: ProgramInfo, val fnInstructions: mutable.ListBuffer[LLVMItem]) {
  def gen(e: Exp): LocalIdentifier =
    e match
      case e: BinOp =>
        val lhs = gen(e.lhs)
        val rhs = gen(e.rhs)
        pInfo.localIdCounter += 1
        val res = LocalIdentifier(pInfo.localIdCounter.toString)
        e.op match
          case Op.GT | Op.LT | Op.GE | Op.LE | Op.EQ | Op.NE=>
            val cmp = e.op match
              case Op.GT => CmpCode.SGT
              case Op.LT => CmpCode.SLT
              case Op.GE => CmpCode.SGE
              case Op.LE => CmpCode.SLE
              case Op.EQ => CmpCode.EQ
              case Op.NE => CmpCode.NE
            fnInstructions.addOne(Instruction.Compare(res, cmp, convertType(e.lhs.eType), lhs, rhs))
          case _ =>
            val op = e.op match
              case Op.ADD => OpCode.ADD
              case Op.SUB => OpCode.SUB
              case Op.MUL => OpCode.MUL
              case Op.MOD => OpCode.SREM
              case Op.OR => OpCode.OR
              case Op.AND => OpCode.AND
            fnInstructions.addOne(Instruction.BinaryInstruction(res, op, convertType(e.lhs.eType), lhs, rhs))
        res
      case e: StrLiteral => null
      case e: IntLiteral => null
      case e: Var => null
      case e: Apply => null
      case e: InitClosure => null
      case e: InitArr => null
      case e: InitStruct => null
      case e: StructArrLiteral => null
      case e: GetElementAt => null
      case e: SetElementAt => null
      case e: Phi => null
      case VoidE => null

}