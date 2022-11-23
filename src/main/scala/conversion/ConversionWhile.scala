package conversion

import scala.collection.mutable

class ConversionWhile(ir: IR) {
  private def createWhileFn(whileLoop: While, params: List[Var]): Assign =
    ir.varCounter += 1
    val recName = ir.varCounter.toString
    val recBlock = Block()

    def convertContinueBreak(s: Stmt): Stmt =
      s match
        case s: Continue => Return(Apply(Var(recName), params))
        case s: Break => Block(whileLoop.tail.stmts.toList.appended(Return(VoidE)))
        case s: Assign => s
        case s: While => s
        case s: If => If(s.cond, convertContinueBreak(s.bThen).asInstanceOf[Block], convertContinueBreak(s.bElse).asInstanceOf[Block])
        case s: Block => Block(s.stmts.map(convertContinueBreak))
        case s: Exp => s

    // TODO: rename params

    val tailReturn = Return(Apply(Var(recName), params))
    recBlock.stmts.add(
      If(whileLoop.cond,
        convertContinueBreak(whileLoop.tail).asInstanceOf[Block],
        Block(convertContinueBreak(whileLoop.body), tailReturn)))

    Assign(ir.varCounter.toString,
      Rec(ir.varCounter.toString,
        Fn(params, recBlock, tp=IRFunction(tailReturn.value.eType, params.map(p => p.eType))
        )
      )
    )



  def convertWhile(s: Stmt, parentBlock: Block, params: mutable.ArrayBuffer[Var], insideFun: Boolean = false): Unit =
    s match
      case s: While =>
        val recAssignment = createWhileFn(s, params.toList)
        ir.prog.stmts.add(recAssignment)
        parentBlock.stmts.insertAfter(s,
          Apply(
            Var(recAssignment.name, recAssignment.value.eType),
            params.map(v => Var(v.name, v.eType)).toList
          )
        )
        parentBlock.stmts.remove(s)
        convertWhile(recAssignment, ir.prog, mutable.ArrayBuffer.empty[Var])

      case s: Assign => if insideFun then params.addOne(Var(s.name, s.value.eType)); convertWhile(s.value, parentBlock, params)
      case s: If =>
        convertWhile(s.cond, parentBlock, params, insideFun)
        val ifParams = mutable.ArrayBuffer.from(params)
        val elseParams = mutable.ArrayBuffer.from(params)
        convertWhile(s.bThen, parentBlock, ifParams, insideFun)
        convertWhile(s.bElse, parentBlock, elseParams, insideFun)
      case s: Block => ;
        s.stmts.foreach(s_ => convertWhile(s_, s, params, insideFun))
      case s: Return =>
        convertWhile(s.value, parentBlock, params, insideFun)
      case s: SetElementAt =>
        convertWhile(s.array, parentBlock, params, insideFun)
        convertWhile(s.index, parentBlock, params, insideFun)
        convertWhile(s.elm, parentBlock, params, insideFun)
      case s: BinOp => ;
        convertWhile(s.lhs, parentBlock, params, insideFun)
        convertWhile(s.rhs, parentBlock, params, insideFun)
      case s: (StrLiteral | IntLiteral | Var) => ;
      case s: Fn =>
        val newParams = mutable.ArrayBuffer.from(params)
        newParams.addAll(s.params)
        convertWhile(s.body, s.body, newParams, true)
      case s: Rec =>
        convertWhile(s.fn, parentBlock, params, insideFun);
      case s: Apply =>
        convertWhile(s.fn, parentBlock, params, insideFun)
        s.args.foreach(e => convertWhile(e, parentBlock, params, insideFun))
      case s: InitArr =>
        convertWhile(s.size, parentBlock, params, insideFun)
      case _: InitStruct => ;
      case s: StructArrLiteral =>
        s.elements.foreach( e => convertWhile(e, parentBlock, params, insideFun))
      case s: GetElementAt =>
        convertWhile(s.index, parentBlock, params, insideFun)
        convertWhile(s.array, parentBlock, params, insideFun)
      case s: GetElementAt =>
        convertWhile(s.index, parentBlock, params, insideFun)
        convertWhile(s.array, parentBlock, params, insideFun)
        convertWhile(s.elm, parentBlock, params, insideFun)
      case VoidE => ;

  private val initialParams = mutable.ArrayBuffer.empty[String]
  ir.prog.stmts.foreach(s => convertWhile(s, ir.prog, initialParams))
}
