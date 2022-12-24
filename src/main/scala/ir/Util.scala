package ir

import scala.collection.mutable

object Util {
  /**
   * Find the used variables in an expression.
   * They are the union of {name: Var(name) in e} and findFreeVars(fn) for each fn: Fn in e.
   * @param e Any expression
   * @return The set of variables names that are used in e
   */
  def findVarsUsed(e: Exp): mutable.Set[NameTypePair] =
    val varsUsed = mutable.HashSet.empty[NameTypePair]
    def findVarsUsed_(e: Exp): Unit =
      e match
        case e: InitClosure => varsUsed.add(e.fn); varsUsed.addAll(e.env)
        case e: BinOp => findVarsUsed_(e.lhs); findVarsUsed_(e.rhs);
        case _: IntLiteral => ;
        case _: StrLiteral => ;
        case e: Var => varsUsed.add(NameTypePair(e.name, e.eType));
        case e: Fn => varsUsed.addAll(findFreeVars(e));
        case e: Rec => findVarsUsed_(e.fn);
        case e: Apply => findVarsUsed_(e.fn); e.args.foreach(findVarsUsed_);
        case e: InitArr => findVarsUsed_(e.size);
        case _: InitStruct => ;
        case e: StructArrLiteral => e.elements.foreach(findVarsUsed_);
        case e: GetElementAt => findVarsUsed_(e.array); findVarsUsed_(e.index);
        case e: SetElementAt => findVarsUsed_(e.array); findVarsUsed_(e.index); findVarsUsed_(e.elm);
        case VoidE => ;

    findVarsUsed_(e)
    varsUsed

  /**
   * Find the free variables in a Fn expression
   * @param e any Fn expression
   * @return set of free variables in e
   */
  def findFreeVars(e: Fn): mutable.Set[NameTypePair] =
    val freeVars = mutable.HashSet.empty[NameTypePair]
    val declaredVars = mutable.HashSet.empty[NameTypePair]
    declaredVars.addAll(e.params)

    def findFreeVars_(e: Stmt): Unit =
      e match
        case e: InitClosure => e.env.foreach(
          pair => if !declaredVars.contains(pair) then freeVars.add(pair)
        );
        case e: Assign => findFreeVars_(e.value); declaredVars.add(NameTypePair(e.name, e.value.eType));
        case e: Block => e.stmts.foreach(findFreeVars_);
        case e: If => findFreeVars_(e.cond); findFreeVars_(e.bThen); findFreeVars_(e.bElse);
        case e: Return => findFreeVars_(e.value);
        case e: BinOp => findFreeVars_(e.lhs); findFreeVars_(e.rhs);
        case _: StrLiteral => ;
        case _: IntLiteral => ;
        case e: Var => if !declaredVars.contains(NameTypePair(e.name, e.eType)) then freeVars.add(NameTypePair(e.name, e.eType));
        case e: Fn => findFreeVars(e).diff(declaredVars)
        case e: Rec => findFreeVars_(e.fn);
        case e: Apply => findFreeVars_(e.fn); e.args.foreach(findFreeVars_);
        case e: InitArr => findFreeVars_(e.size);
        case _: InitStruct => ;
        case e: StructArrLiteral => e.elements.foreach(findFreeVars_);
        case e: GetElementAt => findFreeVars_(e.array); findFreeVars_(e.index);
        case e: SetElementAt => findFreeVars_(e.array); findFreeVars_(e.index); findFreeVars_(e.elm);
        case VoidE => ;

    findFreeVars_(e.body)
    freeVars

  def findGlobalVars(s: Stmt): mutable.Set[NameTypePair] =
    val globalVars = mutable.Set.empty[NameTypePair]
    def findGlobalVars_(s: Stmt): Unit =
      s match
        case s: If => findGlobalVars_(s.bThen); findGlobalVars_(s.bElse)
        case s: Block => s.stmts.foreach(findGlobalVars_)
        case s: Assign => globalVars.add(NameTypePair(s.name, s.value.eType))
        case s: Return => ;
        case s: Exp => ;
        
    findGlobalVars_(s)
    globalVars
}
