package conversion

import scala.collection.mutable

class ConversionWhile(ir: IR) {
  private def createWhileFn(whileLoop: While, params: List[NameTypePair], retType: IRType): Assign =
    ir.varCounter += 1
    val recName = ir.varCounter.toString
    val recBlock = Block()
    val recFn = Rec(recName,
      Fn(params, recBlock, retType)
    )

    def convertContinueBreak(s: Stmt): Stmt =
      s match
        case s: Continue => Return(Apply(Var(recName), params.map(pair => Var(pair.name, pair.tp))))
        case s: Break => Block(whileLoop.tail.stmts.toList.appended(Return(VoidE)))
        case s: Assign => s
        case s: While => s
        case s: Return => s
        case s: If => If(s.cond, convertContinueBreak(s.bThen).asInstanceOf[Block], convertContinueBreak(s.bElse).asInstanceOf[Block])
        case s: Block => Block(s.stmts.map(convertContinueBreak))
        case s: Exp => s

    // TODO: rename params

    val tailReturn = Return(
      Apply(
        Var(recName, recFn.eType),
        params.map(pair => Var(pair.name, pair.tp))
      )
    )
    recBlock.stmts.add(
      If(whileLoop.cond,
        convertContinueBreak(whileLoop.tail).asInstanceOf[Block],
        Block(convertContinueBreak(whileLoop.body), tailReturn)))

    Assign(ir.varCounter.toString,
      recFn
    )



  def convertWhile(s: Stmt,
                   parentBlock: Block,
                   params: mutable.ArrayBuffer[NameTypePair],
                   insideFun: Boolean = false,
                   retType: IRType = Undefined): Unit =
    s match
      case s: While =>
        val recAssignment = createWhileFn(s, params.toList, retType)
        ir.code.stmts.add(recAssignment)
        parentBlock.stmts.insertAfter(s,
          Apply(
            Var(recAssignment.name, recAssignment.value.eType),
            params.map(p => Var(p.name, p.tp)).toList
          )
        )
        parentBlock.stmts.remove(s)
        convertWhile(recAssignment, ir.code, mutable.ArrayBuffer.empty[NameTypePair], insideFun, retType)

      case s: Assign =>
        if insideFun then params.addOne(NameTypePair(s.name, s.value.eType))
        convertWhile(s.value, parentBlock, params, insideFun, retType)
      case s: If =>
        convertWhile(s.cond, parentBlock, params, insideFun, retType)
        val ifParams = mutable.ArrayBuffer.from(params)
        val elseParams = mutable.ArrayBuffer.from(params)
        convertWhile(s.bThen, parentBlock, ifParams, insideFun, retType)
        convertWhile(s.bElse, parentBlock, elseParams, insideFun, retType)
      case s: Block => ;
        s.stmts.foreach(s_ => convertWhile(s_, s, params, insideFun, retType))
      case s: Return =>
        convertWhile(s.value, parentBlock, params, insideFun, retType)
      case s: BinOp => ;
        convertWhile(s.lhs, parentBlock, params, insideFun, retType)
        convertWhile(s.rhs, parentBlock, params, insideFun, retType)
      case s: (StrLiteral | IntLiteral | Var) => ;
      case s: Fn =>
        val newParams = mutable.ArrayBuffer.from(params)
        newParams.addAll(s.params)
        convertWhile(s.body, s.body, newParams, true, s.eType match
          case IRFunction(retType, _) => retType
          case _ => Undefined
        )
      case s: Rec =>
        convertWhile(s.fn, parentBlock, params, insideFun, retType);
      case s: Apply =>
        convertWhile(s.fn, parentBlock, params, insideFun, retType)
        s.args.foreach(e => convertWhile(e, parentBlock, params, insideFun, retType))
      case s: InitArr =>
        convertWhile(s.size, parentBlock, params, insideFun, retType)
      case _: InitStruct => ;
      case s: StructArrLiteral =>
        s.elements.foreach( e => convertWhile(e, parentBlock, params, insideFun, retType))
      case s: GetElementAt =>
        convertWhile(s.index, parentBlock, params, insideFun, retType)
        convertWhile(s.array, parentBlock, params, insideFun, retType)
      case s: SetElementAt =>
        convertWhile(s.index, parentBlock, params, insideFun, retType)
        convertWhile(s.array, parentBlock, params, insideFun, retType)
        convertWhile(s.elm, parentBlock, params, insideFun, retType)
      case s: ConvertInt => convertWhile(s.int, parentBlock, params, insideFun, retType)
      case VoidE => ;

  private val initialParams = mutable.ArrayBuffer.empty[NameTypePair]
  ir.code.stmts.foreach(s => convertWhile(s, ir.code, initialParams))
}
