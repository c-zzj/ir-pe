package conversion
import scala.collection.mutable


class TypeAnalyzer(ir: IR) {
  /**
   * complete type inference on an expression.
   * @param exp expression to analyze
   * @throws TypeError when type inference is impossible
   */
  def analyzeExp(exp: Exp, nameTypeMap: mutable.HashMap[String, IRType]): Unit =
    exp match
      case e: BinOp =>
        analyzeExp(e.lhs, nameTypeMap)
        analyzeExp(e.rhs, nameTypeMap)
        if (e.lhs.eType != e.rhs.eType)
          throw TypeError("BinOp operands do not have the same type")
        e.op match
          case Op.GT | Op.LT | Op.GE | Op.LE | Op.EQ | Op.NE => e.tp = IRInt(1)
          case _ => e.tp = e.lhs.eType
      case e: (StrLiteral | IntLiteral) => ;
      case e: Var => e.tp = nameTypeMap(e.name);
      case e: Fn =>
        val newNameTypeMap = mutable.HashMap.from(nameTypeMap)
        newNameTypeMap.addAll(e.params.map(pair => (pair.name, pair.tp)))
        val retType = analyzeStmt(e.body, newNameTypeMap)
        if retType == Undefined then throw TypeError("function never returns")
        if e.retType != retType then throw TypeError("function has wrong return type")
      case e: Rec => analyzeExp(e.fn, nameTypeMap)
      case e: Apply =>
        analyzeExp(e.fn, nameTypeMap)
        e.args.foreach(e => analyzeExp(e, nameTypeMap))
      case e: InitArr =>
        analyzeExp(e.size, nameTypeMap)
        e.size.eType match
          case IRInt(32) => ;
          case _ => throw TypeError("InitArr requires size to be of type Int 32")
      case e: InitStruct => ;
      case e: StructArrLiteral => e.elements.foreach(exp => analyzeExp(exp, nameTypeMap))
      case e: GetElementAt => analyzeExp(e.index, nameTypeMap); analyzeExp(e.array, nameTypeMap)
      case e: SetElementAt => analyzeExp(e.elm, nameTypeMap); analyzeExp(e.index, nameTypeMap); analyzeExp(e.array, nameTypeMap)
      case e: ConvertInt => analyzeExp(e.int, nameTypeMap)
      case VoidE => ;

  /**
   * complete type inference on a statement.
   *
   * @param stmt statement to analyze
   * @return the return type of the statement
   * @throws TypeError when type inference is impossible
   */
  def analyzeStmt(stmt: Stmt, nameTypeMap: mutable.HashMap[String, IRType]): IRType =
    stmt match
      case stmt: While =>
        analyzeExp(stmt.cond, nameTypeMap)
        val whileRet = analyzeStmt(stmt.body, nameTypeMap)
        val tailRet = analyzeStmt(stmt.tail, nameTypeMap)
        if whileRet != Undefined && tailRet != Undefined && whileRet != tailRet
        then throw TypeError("Inconsistent return type in While stmt")
        else if whileRet != Undefined then whileRet
        else if tailRet != Undefined then tailRet
        else Undefined
      case stmt: (Continue | Break) => ; Undefined
      case stmt: Assign =>
        stmt.value match
          case _: (Fn | Rec) =>
            nameTypeMap.put(stmt.name, stmt.value.eType)
            analyzeExp(stmt.value, nameTypeMap)
          case _ =>
            analyzeExp(stmt.value, nameTypeMap)
            nameTypeMap.put(stmt.name, stmt.value.eType)
        Undefined
      case stmt: If =>
        analyzeExp(stmt.cond, nameTypeMap)
        val thenReturn = analyzeStmt(stmt.bThen, nameTypeMap)
        val elseReturn = analyzeStmt(stmt.bElse, nameTypeMap)
        if thenReturn != Undefined && elseReturn != Undefined && thenReturn != elseReturn
          then throw TypeError("Inconsistent return type in If stmt")
        else if thenReturn != Undefined then thenReturn
        else if elseReturn != Undefined then elseReturn
        else Undefined
      case stmt: Block =>
        val types = stmt.stmts.toList.map(s => analyzeStmt(s, nameTypeMap)).filterNot(t => t == Undefined)
        if types.isEmpty then Undefined
        else if types.forall(t => t == types.head) then types.head
        else throw TypeError("Inconsistent return type in Block stmt")
      case stmt: Return => analyzeExp(stmt.value, nameTypeMap); stmt.value.eType
      case e: Exp => analyzeExp(e, nameTypeMap); Undefined

  private val nameTypeMap = mutable.HashMap.empty[String, IRType]

  def addBuiltinFnTypes(): Unit =
    nameTypeMap.put("puts", IRFunction(IRVoid, List(IRArray(IRInt(8)))))
    nameTypeMap.put("putchar", IRFunction(IRVoid, List(IRInt(8))))
    nameTypeMap.put("itoa", IRFunction(IRArray(IRInt(8)), List(IRInt(32))))

  addBuiltinFnTypes()
  ir.code.stmts.foreach(s => analyzeStmt(s, nameTypeMap))
}

final case class TypeError(private val message: String = "",
                                private val cause: Throwable = None.orNull)
  extends Exception(message, cause)