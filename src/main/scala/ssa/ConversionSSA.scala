package ssa

import scala.collection.mutable

class ConversionSSA(program: Stmt) {
  private def find_global_vars(): Unit =
    for (block <- cfg.blocks) {
      val varkill = mutable.HashSet.empty[String]
      for (stmt <- block.elements){
        val addGlobal = (name: String) => if ! varkill.contains(name) then globals.add(name)
        stmt match
          case stmt: Let =>
            vars_used(stmt.value).foreach(addGlobal)
            varkill.add(stmt.name)
            val var_blocks = name_block_map.getOrElseUpdate(stmt.name, mutable.HashSet.empty[BlockNode])
            var_blocks.add(block)
          case stmt: If => vars_used(stmt.cond).foreach(addGlobal);
          case stmt: Return => vars_used(stmt.value).foreach(addGlobal);
          case stmt: Exp => vars_used(stmt).foreach(addGlobal);
          case _: Block => throw IllegalStateException("Block stmt should not be in the CFG");
      }

    }

  val cfg: ControlFlowGraph = ControlFlowGraph(program)

  val name_block_map: mutable.Map[String, mutable.Set[BlockNode]] = mutable.HashMap.empty[String, mutable.Set[BlockNode]]

  val globals: mutable.Set[String] = mutable.HashSet.empty[String]

  find_global_vars()
}

def vars_used(e: Exp): mutable.Set[String] =
  val varsUsed = mutable.HashSet.empty[String]
  def vars_used_(e: Exp): Unit =
    e match
      case e: BinOp => vars_used_(e.lhs); vars_used_(e.rhs);
      case _: IntLiteral => ;
      case _: StrLiteral => ;
      case e: Var => varsUsed.add(e.name);
      case e: Fn => varsUsed.addAll(free_vars(e));
      case e: Rec => vars_used_(e.fn);
      case e: Apply => vars_used_(e.fn); e.args.foreach(vars_used_);
      case e: Build => vars_used_(e.fn); vars_used_(e.size);
      case e: Arr => e.elements.foreach(vars_used_);
      case e: ReadArr => vars_used_(e.array); vars_used_(e.index);
      case UnitE => ;

  vars_used_(e)
  varsUsed

def free_vars(e: Fn): mutable.Set[String] =
  val freeVars = mutable.HashSet.empty[String]
  val declaredVars = mutable.HashSet.empty[String]
  declaredVars.addAll(e.params.iterator)
  def free_vars_(e: Stmt): Unit =
    e match
      case e: Let => free_vars_(e.value); declaredVars.add(e.name);
      case e: Block => e.stmts.foreach(free_vars_);
      case e: If => free_vars_(e.cond); free_vars_(e.bThen); free_vars_(e.bElse);
      case e: Return => free_vars_(e.value);
      case e: BinOp => free_vars_(e.lhs); free_vars_(e.rhs);
      case _: StrLiteral => ;
      case _: IntLiteral => ;
      case e: Var => if !declaredVars.contains(e.name) then freeVars.add(e.name);
      case _: Fn => throw IllegalStateException();
      case _: Rec => throw IllegalStateException();
      case e: Apply => free_vars_(e.fn); e.args.foreach(free_vars_);
      case e: Build => free_vars_(e.fn); free_vars_(e.size);
      case e: Arr => e.elements.foreach(free_vars_);
      case e: ReadArr => free_vars_(e.array); free_vars_(e.index);
      case UnitE => ;

  free_vars_(e.body)
  freeVars