package ir

import scala.collection.mutable

class ConversionClosure(ir: IR) {
  def convertClosure(function: Fn | Rec): Unit =

    /**
     * replace variables in the environments in the closure with access to the environment struct (last argument)
     * @param env the environment
     * @param fn the closure
     */
    def rename(env: List[NameTypePair], fn: Fn | Rec, envArg: NameTypePair, envVarPosMap: mutable.Map[String, Int]): Unit =
      val f = fn match
        case e: Rec => e.fn
        case e: Fn => e
      rename_(f.body)

      def rename_(e: Stmt): Stmt =
        e match
          case e: Assign => e.value = rename_(e.value).asInstanceOf[Exp]; e
          case e: Block => e.stmts = e.stmts.map(rename_); e
          case e: If =>
            e.cond = rename_(e.cond).asInstanceOf[Exp]
            e.bThen = rename_(e.bThen) match
              case b: Block => b
              case b => throw RuntimeException(b.toString + " is not a block")
            e.bElse = rename_(e.bElse) match
              case b: Block => b
              case b => throw RuntimeException(b.toString + " is not a block")
            e
          case e: Return =>
            e.value = rename_(e.value).asInstanceOf[Exp]
            e
          case e: BinOp =>
            e.lhs = rename_(e.lhs).asInstanceOf[Exp]
            e.rhs = rename_(e.rhs).asInstanceOf[Exp]
            e
          case e: StrLiteral => e;
          case e: IntLiteral => e;
          case e: Var =>
            if envVarPosMap.contains(e.name) then
            GetElementAt(Var(envArg.name, envArg.tp), IntLiteral(envVarPosMap(e.name), 32))
            else e
          case e: Apply =>
            e.fn = rename_(e.fn).asInstanceOf[Exp]
            e.args = e.args.map(rename_).map(e_ => e_.asInstanceOf[Exp])
            e
          case e: InitArr =>
            e.size = rename_(e.size).asInstanceOf[Exp]
            e
          case e: InitStruct => e
          case e: StructArrLiteral =>
            e.elements = e.elements.map(rename_).map(e_ => e_.asInstanceOf[Exp])
            e
          case e: GetElementAt =>
            e.array = rename_(e.array).asInstanceOf[Exp]
            e.index = rename_(e.index).asInstanceOf[Exp]
            e
          case e: SetElementAt =>
            e.array = rename_(e.array).asInstanceOf[Exp]
            e.index = rename_(e.index).asInstanceOf[Exp]
            e.index = rename_(e.elm).asInstanceOf[Exp]
            e
          case e: Fn => rename_(e.body).asInstanceOf[Block]; e
          case e: Rec => rename_(e.fn).asInstanceOf[Fn]; e
          case e: ConvertInt => e.int = rename_(e.int).asInstanceOf[Exp]; e
          case VoidE => VoidE;

    /**
     * create a function in global scope for the closure
     *
     * @param e the closure
     * @return the IR representation of the closure
     */
    def createClosure(e: Fn | Rec): StructArrLiteral =
      ir.varCounter += 1;
      val g = ir.varCounter.toString
      ir.code.stmts.prepend(Assign(g, e))
      val f = e match
        case e: Rec => e.fn
        case e: Fn => e
      val env = Util.findFreeVars(f).diff(globalVars)

      ir.varCounter += 1
      val envArgName = ir.varCounter.toString

      val envList = env.map(pair => Var(pair.name, pair.tp)).toList
      val envStruct = StructArrLiteral(envList)
      val res = StructArrLiteral(List(e, envStruct))

      val envVarPosMap: mutable.Map[String, Int] = mutable.HashMap.empty[String, Int]
      var i = 0
      envList.foreach(v => {
        envVarPosMap.put(v.name, i)
        i += 1
      })

      val envArg = NameTypePair(envArgName, envStruct.eType)

      f.params = f.params.appended(envArg)

      rename(env.toList, f, envArg, envVarPosMap)

      res

    /**
     * traverse the statement and convert closures inside
     * @param e statement to be traversed on
     * @return the statement after conversion
     */
    def convertClosure_(e: Stmt): Stmt =
      e match
        case e: Assign => e.value = convertClosure_(e.value).asInstanceOf[Exp]; e
        case e: Block => e.stmts = e.stmts.map(convertClosure_); e
        case e: If =>
          e.cond = convertClosure_(e.cond).asInstanceOf[Exp]
          e.bThen = convertClosure_(e.bThen) match
            case b: Block => b
            case b => throw RuntimeException(b.toString + " is not a block")
          e.bElse = convertClosure_(e.bElse) match
            case b: Block => b
            case b => throw RuntimeException(b.toString + " is not a block")
          e
        case e: Return =>
          e.value = convertClosure_(e.value).asInstanceOf[Exp]
          e
        case e: BinOp =>
          e.lhs = convertClosure_(e.lhs).asInstanceOf[Exp]
          e.rhs = convertClosure_(e.rhs).asInstanceOf[Exp]
          e
        case e: StrLiteral => e;
        case e: IntLiteral => e;
        case e: Var => e;
        case e: Apply =>
          e.fn = convertClosure_(e.fn).asInstanceOf[Exp]
          e.args = e.args.map(convertClosure_).map(e_ => e_.asInstanceOf[Exp])
          e
        case e: InitArr =>
          e.size = convertClosure_(e.size).asInstanceOf[Exp]
          e
        case e: InitStruct => e
        case e: StructArrLiteral =>
          e.elements = e.elements.map(convertClosure_).map(e_ => e_.asInstanceOf[Exp])
          e
        case e: GetElementAt =>
          e.array = convertClosure_(e.array).asInstanceOf[Exp]
          e.index = convertClosure_(e.index).asInstanceOf[Exp]
          e
        case e: SetElementAt =>
          e.array = convertClosure_(e.array).asInstanceOf[Exp]
          e.index = convertClosure_(e.index).asInstanceOf[Exp]
          e.index = convertClosure_(e.elm).asInstanceOf[Exp]
          e
        case e: Fn=> val closure = createClosure(e); convertClosure(e); closure
        case e: Rec => val closure = createClosure(e); convertClosure(e); closure
        case e: ConvertInt => convertClosure_(e.int); e
        case VoidE => VoidE;


    function match
      case function: Fn => convertClosure_(function.body)
      case function: Rec => convertClosure_(function.fn.body)


  /**
   * Apply convertClosure to globally defined functions
   * @param e a global Stmt
   */
  def convertGlobal(e: Stmt): Unit =
    e match
      case e: Assign => convertGlobal(e.value);
      case e: Block => e.stmts.foreach(convertGlobal);
      case e: If => convertGlobal(e.cond); convertGlobal(e.bThen); convertGlobal(e.bElse);
      case e: Return => convertGlobal(e.value);
      case e: BinOp => convertGlobal(e.lhs); convertGlobal(e.rhs);
      case _: StrLiteral => ;
      case _: IntLiteral => ;
      case _: Var => ;
      case e: Fn => convertClosure(e);
      case e: Rec => convertClosure(e.fn);
      case e: Apply => convertGlobal(e.fn); e.args.foreach(convertGlobal);
      case e: InitArr => convertGlobal(e.size);
      case _: InitStruct => ;
      case e: StructArrLiteral => e.elements.foreach(convertGlobal);
      case e: GetElementAt => convertGlobal(e.array); convertGlobal(e.index);
      case e: SetElementAt => convertGlobal(e.array); convertGlobal(e.index); convertGlobal(e.elm);
      case e: ConvertInt => convertGlobal(e.int);
      case VoidE => ;

  def convertApply(e: Stmt): Unit =
    e match
      case e: Assign => convertApply(e.value);
      case e: Block => e.stmts.foreach(convertApply);
      case e: If => convertApply(e.cond); convertApply(e.bThen); convertApply(e.bElse);
      case e: Return => convertApply(e.value);
      case e: BinOp => convertApply(e.lhs); convertApply(e.rhs);
      case _: StrLiteral => ;
      case _: IntLiteral => ;
      case _: Var => ;
      case e: Fn => convertApply(e.body);
      case e: Rec => convertApply(e.fn);
      case e: Apply =>
        e.args.foreach(convertApply)
        convertApply(e.fn);
        e.fn.eType match
        case IRStruct(_) =>
          e.fn = GetElementAt(e.fn, IntLiteral(0, 32))
          e.args = e.args.appended(GetElementAt(e.fn, IntLiteral(1, 32)))
        case IRFunction(_, _) => ;

      case e: InitArr => convertApply(e.size);
      case _: InitStruct => ;
      case e: StructArrLiteral => e.elements.foreach(convertApply);
      case e: GetElementAt => convertApply(e.array); convertApply(e.index);
      case e: SetElementAt => convertApply(e.array); convertApply(e.index); convertApply(e.elm);
      case e: ConvertInt => convertApply(e.int)
      case VoidE => ;

  val globalVars: mutable.Set[NameTypePair] = Util.findGlobalVars(ir.code)

  convertGlobal(ir.code)
  convertApply(ir.code)
}

class InitClosure(var env: List[NameTypePair],
                  var fn: NameTypePair) extends Exp :
  override def eType: IRType = fn.tp match
    case t: IRFunction => IRClosure(t.retType, t.argTypeList)
    case t: IRClosure => IRClosure(t.retType, t.argTypeList)
    case _ => Undefined

case class IRClosure(retType: IRType, argTypeList: List[IRType]) extends IRType