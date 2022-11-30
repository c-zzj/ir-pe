package conversion

import scala.collection.mutable

class ConversionClosure(ir: IR) {
  def convertClosure(function: Fn | Rec): Unit =

    def rename(env: List[NameTypePair], fn: Fn | Rec): Unit =
      val nameMap = mutable.HashMap.empty[String, String]
      var envRenamed = List.empty[NameTypePair]
      env.foreach(pair => {
        pair match
          case NameTypePair(name, tp) =>
            ir.varCounter += 1
            val newName = ir.varCounter.toString
            envRenamed = envRenamed.appended(NameTypePair(newName, tp))
            nameMap.put(name, newName)
      })
      val f = fn match
        case e: Rec => e.fn
        case e: Fn => e

      f.env = envRenamed
      rename_(f.body)

      def rename_(e: Stmt): Unit =
        e match
          case e: Assign => rename_(e.value);
          case e: Block => e.stmts.foreach(rename_);
          case e: If => rename_(e.cond); rename_(e.bThen); rename_(e.bElse);
          case e: Return => rename_(e.value);
          case e: BinOp => rename_(e.lhs); rename_(e.rhs);
          case _: StrLiteral => ;
          case _: IntLiteral => ;
          case e: Var => if nameMap.contains(e.name) then e.name = nameMap(e.name);
          case e: Fn => rename_(e.body);
          case e: Rec => rename_(e.fn);
          case e: Apply => rename_(e.fn); e.args.foreach(rename_);
          case e: InitArr => rename_(e.size);
          case _: InitStruct => ;
          case e: StructArrLiteral => e.elements.foreach(rename_);
          case e: GetElementAt => rename_(e.array); rename_(e.index);
          case e: SetElementAt => rename_(e.array); rename_(e.index); rename_(e.elm)
          case VoidE => ;

    def getClosure(e: Fn | Rec): InitClosure =
      ir.varCounter += 1;
      val g = ir.varCounter.toString
      ir.code.stmts.prepend(Assign(g, e))
      val f = e match
        case e: Rec => e.fn
        case e: Fn => e
      val env = Util.findFreeVars(f).diff(globalVars)
      rename(env.toList, f)
      convertClosure(e)
      InitClosure(env.toList, NameTypePair(g, e.eType))

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
        case e: Fn=> getClosure(e)
        case e: Rec => getClosure(e)
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
      case VoidE => ;


  val globalVars: mutable.Set[NameTypePair] = Util.findGlobalVars(ir.code)

  convertGlobal(ir.code)
}
