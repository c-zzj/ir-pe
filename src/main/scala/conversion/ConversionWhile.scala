package conversion

import scala.collection.mutable

class ConversionWhile(ir: IR) {
  private def getWhileFn(whileLoop: While, params: List[String]): Assign =
    ir.varCounter += 1
    val recName = ir.varCounter.toString
    val recBlock = Block()

    def convertContinueBreak(s: Stmt): Stmt =
      s match
        case s: Continue => Return(Apply(Var(recName), params.map(param => Var(param))))
        case s: Break => Block(whileLoop.tail.stmts.toList.appended(Return(UnitE)))
        case s: Assign => s
        case s: While => s
        case s: If => If(s.cond, convertContinueBreak(s.bThen).asInstanceOf[Block], convertContinueBreak(s.bElse).asInstanceOf[Block])
        case s: Block => Block(s.stmts.map(convertContinueBreak))
        case s: Exp => s

    // TODO: rename params

    recBlock.stmts.add(
      If(whileLoop.cond,
        convertContinueBreak(whileLoop.tail).asInstanceOf[Block],
        Block(convertContinueBreak(whileLoop.body), Return(Apply(Var(recName), params.map(param => Var(param)))))))

    Assign(ir.varCounter.toString, Rec(ir.varCounter.toString, Fn(params, recBlock)))



  def convertWhile(s: Stmt, parentBlock: Block, params: mutable.ArrayBuffer[String], insideFun: Boolean = false): Unit =
    s match
      case s: While =>
        val recAssignment = getWhileFn(s, params.toList)
        ir.prog.stmts.add(recAssignment)
        parentBlock.stmts.insertAfter(s, Apply(Var(recAssignment.name), params.map(name => Var(name)).toList))
        parentBlock.stmts.remove(s)
        convertWhile(recAssignment, ir.prog, mutable.ArrayBuffer.empty[String])

      case s: Assign => if insideFun then params.addOne(s.name); convertWhile(s.value, parentBlock, params)
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
      case s: BinOp => ;
        convertWhile(s.lhs, parentBlock, params, insideFun)
        convertWhile(s.rhs, parentBlock, params, insideFun)
      case s: (ChrLiteral | IntLiteral | Var) => ;
      case s: Fn =>
        val newParams = mutable.ArrayBuffer.from(params)
        newParams.addAll(s.params)
        convertWhile(s.body, s.body, newParams, true)
      case s: Rec =>
        convertWhile(s.fn, parentBlock, params, insideFun);
      case s: Apply =>
        convertWhile(s.fn, parentBlock, params, insideFun)
        s.args.foreach(e => convertWhile(e, parentBlock, params, insideFun))
      case s: Build =>
        convertWhile(s.fn, parentBlock, params, insideFun)
        convertWhile(s.size, parentBlock, params, insideFun)
      case s: Arr =>
        s.elements.foreach( e => convertWhile(e, parentBlock, params, insideFun))
      case s: ReadArr =>
        convertWhile(s.index, parentBlock, params, insideFun)
        convertWhile(s.array, parentBlock, params, insideFun)
      case UnitE => ;

  private val initialParams = mutable.ArrayBuffer.empty[String]
  ir.prog.stmts.foreach(s => convertWhile(s, ir.prog, initialParams))
}
