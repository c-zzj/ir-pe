package gen

import gen.llvm.*
import ir.*
import gen.CodegenUtil.ShouldNotReach
import gen.llvm.Instruction.Unreachable
import gen.llvm.LLType.{TInt, TOpaquePtr, TStruct, TVoid}

import collection.mutable

class FunGen(val pInfo: ProgramInfo) {
  def gen(section: Section, fn: Fn, fnName: String): Unit =
    val fnInstructions = mutable.ListBuffer.empty[LLVMItem]
    val expGen = ExpGen(pInfo, section, fnInstructions)
    fn.params.foreach(pair => pInfo.varIdMap.put(pair.name, pInfo.getLocalId))
    pInfo.varIdMap.put(fnName, GlobalIdentifier(fnName))

    def gen_(stmt: Stmt): Unit =
      stmt match
        case s: Assign =>
          val value = expGen.gen(s.value)
          value match
            case VoidConstant => throw CodegenUtil.ShouldNotReach()
            case _ => pInfo.varIdMap.put(s.name, value)
        case s: Block =>
          s.stmts.foreach(gen_)
        case s: If =>
          val value = expGen.gen(s.cond)

          val thenLabel = Label()
          val elseLabel = Label()
          val exitLabel = Label()
          fnInstructions.addOne(Instruction.CondBranch(value, thenLabel, elseLabel))

          pInfo.blockLabelMap.put(s.bThen, thenLabel)
          fnInstructions.addOne(thenLabel)
          gen_(s.bThen)
          fnInstructions.addOne(Instruction.Branch(exitLabel))

          pInfo.blockLabelMap.put(s.bElse, elseLabel)
          fnInstructions.addOne(elseLabel)
          gen_(s.bElse)
          fnInstructions.addOne(Instruction.Branch(exitLabel))

          fnInstructions.addOne(exitLabel)

        case s: Return =>
          val value = expGen.gen(s.value)
          fnInstructions.addOne(Instruction.Return(CodegenUtil.convertType(s.value.eType), value))
        case s: Exp =>
          expGen.gen(s)

    gen_(fn.body)
    fnInstructions.addOne(Unreachable) // add one instruction at the end for If statement to jump out to
    fn.eType match
      case IRFunction(retType, _) =>
        section.globals.addOne(FunDef(
          CodegenUtil.convertType(retType),
          pInfo.varIdMap(fnName).asInstanceOf[GlobalIdentifier],
          fn.params.map(pair => (CodegenUtil.convertType(pair.tp), pInfo.varIdMap(pair.name).asInstanceOf[LocalIdentifier])),
          fnInstructions.toList
        ))
      case _ => throw CodegenUtil.ShouldNotReach()


}

class ExpGen(val pInfo: ProgramInfo, val curSection: Section, val fnInstructions: mutable.ListBuffer[LLVMItem]) {
  def gen(e: Exp): Identifier =
    e match
      case e: BinOp =>
        val lhs = gen(e.lhs)
        val rhs = gen(e.rhs)
        val res = pInfo.getLocalId
        e.op match
          case Op.GT | Op.LT | Op.GE | Op.LE | Op.EQ | Op.NE =>
            val cmp = e.op match
              case Op.GT => CmpCode.SGT
              case Op.LT => CmpCode.SLT
              case Op.GE => CmpCode.SGE
              case Op.LE => CmpCode.SLE
              case Op.EQ => CmpCode.EQ
              case Op.NE => CmpCode.NE
            fnInstructions.addOne(Instruction.Compare(res, cmp, CodegenUtil.convertType(e.lhs.eType), lhs, rhs))
          case _ =>
            val op = e.op match
              case Op.ADD => OpCode.ADD
              case Op.SUB => OpCode.SUB
              case Op.MUL => OpCode.MUL
              case Op.DIV => OpCode.SDIV
              case Op.MOD => OpCode.SREM
              case Op.OR => OpCode.OR
              case Op.AND => OpCode.AND
            fnInstructions.addOne(Instruction.BinaryInstruction(res, op, CodegenUtil.convertType(e.lhs.eType), lhs, rhs))
        res
      case e: StrLiteral =>
        val res = pInfo.getGlobalId
        curSection.globals.addOne(GlobalVar(res, CodegenUtil.convertConstant(e)))
        res
      case e: IntLiteral => CodegenUtil.convertConstant(e)
      case e: Var => pInfo.varIdMap(e.name)
      case e: Apply =>
        val fn = gen(e.fn)
        val args = e.args.map(gen)

        e.fn.eType match
          case IRFunction(retType, argTypeList) =>
            if CodegenUtil.convertType(retType) == TVoid then
              fnInstructions.addOne(
                Instruction.FunCall(
                  None,
                  CodegenUtil.convertType(retType),
                  fn,
                  argTypeList.map(CodegenUtil.convertType) zip args
                )
              )
              VoidConstant
            else
              val res = pInfo.getLocalId
              fnInstructions.addOne(
                Instruction.FunCall(
                  Some(res),
                  CodegenUtil.convertType(retType),
                  fn,
                  argTypeList.map(CodegenUtil.convertType) zip args
                )
              )
              res
          case _ => throw CodegenUtil.ShouldNotReach()
      case e: InitArr =>
        val arraySize = gen(e.size)
        val tmp = pInfo.getLocalId
        val typeSize = pInfo.getLocalId
        fnInstructions.addOne(Instruction.SizeOf(typeSize, CodegenUtil.convertType(e.elmType), tmp))

        val numBytes = pInfo.getLocalId
        fnInstructions.addOne(Instruction.BinaryInstruction(numBytes, OpCode.MUL, TInt(32), typeSize, arraySize))

        val res = pInfo.getLocalId
        fnInstructions.addOne(Instruction.FunCall(Some(res), TOpaquePtr, GlobalIdentifier("malloc"), List((TInt(32), numBytes))))
        res
      case e: InitStruct =>
        val tmp = pInfo.getLocalId
        val typeSize = pInfo.getLocalId
        fnInstructions.addOne(Instruction.SizeOf(typeSize, CodegenUtil.convertContentType(e.eType), tmp))
        val res = pInfo.getLocalId
        fnInstructions.addOne(Instruction.FunCall(Some(res), TOpaquePtr, GlobalIdentifier("malloc"), List((TInt(32), typeSize))))
        res
      case e: StructArrLiteral => null
      case e: GetElementAt =>
        val arrayPtr = gen(e.array)
        val index = gen(e.index)
        val elmPtr = pInfo.getLocalId
        fnInstructions.addOne(Instruction.GetElementPtr(
          elmPtr,
          CodegenUtil.convertType(e.array.eType),
          arrayPtr,
          List((CodegenUtil.convertType(e.index.eType), index))
        ))
        val res = pInfo.getLocalId
        fnInstructions.addOne(Instruction.Load(res, CodegenUtil.convertType(e.eType), elmPtr))
        res
      case e: SetElementAt =>
        val arrayPtr = gen(e.array)
        val index = gen(e.index)
        val elm = gen(e.elm)

        val elmPtr = pInfo.getLocalId
        fnInstructions.addOne(Instruction.GetElementPtr(
          elmPtr,
          CodegenUtil.convertType(e.array.eType),
          arrayPtr,
          List((CodegenUtil.convertType(e.index.eType), index))
        ))
        fnInstructions.addOne(Instruction.Store(CodegenUtil.convertType(e.eType), elm,elmPtr))
        VoidConstant
      case e: Phi =>
        val res = pInfo.getLocalId
        val sources = e.from.toList.map((block: Block, name: String) => (pInfo.varIdMap(name), pInfo.blockLabelMap(block)))
        fnInstructions.addOne(Instruction.Phi(res, CodegenUtil.convertType(e.eType), sources))
        res
      case e: ConvertInt =>
        val from = gen(e.int)
        val res = pInfo.getLocalId
        val op = e.int.eType match
          case IRInt(numBits) => if numBits > e.targetNumBits then OpCode.TRUNC else OpCode.SEXT
          case _ => throw ShouldNotReach("non int source expression in integer conversion")
        fnInstructions.addOne(Instruction.Cast(res, op, CodegenUtil.convertType(e.int.eType), from, CodegenUtil.convertType(e.eType)))
        res
      case VoidE => null

}