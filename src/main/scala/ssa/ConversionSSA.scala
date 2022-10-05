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
            findVarsUsed(stmt.value).foreach(addGlobal)
            varkill.add(stmt.name)
            val var_blocks = nameBlockMap.getOrElseUpdate(stmt.name, mutable.HashSet.empty[BlockNode])
            var_blocks.add(block)
          case stmt: If => findVarsUsed(stmt.cond).foreach(addGlobal);
          case stmt: Return => findVarsUsed(stmt.value).foreach(addGlobal);
          case stmt: Exp => findVarsUsed(stmt).foreach(addGlobal);
          case _: Block => throw IllegalStateException("Block stmt should not be in the CFG");
      }

    }

  private def computeIDomMap(): mutable.Map[BlockNode, BlockNode] =
    val roots: mutable.ListBuffer[BlockNode] = mutable.ListBuffer.empty[BlockNode]
    roots.addOne(cfg.topLevelBlock)
    cfg.funBlockMap.values.foreach(roots.addOne)

    val iDomMap: mutable.Map[BlockNode, BlockNode] = mutable.HashMap.empty[BlockNode, BlockNode]
    val iDomMap_ = mutable.HashMap.empty[BlockNode, mutable.ListBuffer[BlockNode]]

    def traverse(cur: BlockNode): Unit =
      val curAncestors = iDomMap_.getOrElseUpdate(cur, mutable.ListBuffer.empty[BlockNode])
      val curAncestorSet = mutable.HashSet.from(curAncestors)
      for (child <- cur.successors) {
        val childAncestors = iDomMap_.getOrElseUpdate(child, mutable.ListBuffer.empty[BlockNode])
        if childAncestors.isEmpty then
          childAncestors.addAll(curAncestors).addOne(cur)
        else
          for (i <- childAncestors.size - 1 to 0 by -1) {
            if !curAncestorSet.contains(childAncestors(i)) then childAncestors.remove(i)
          }

        traverse(child)
      }

    roots.foreach(traverse)
    iDomMap_.foreach((b, dominators) => iDomMap.put(b, dominators.last))
    iDomMap

  private def computeDominatorFrontiers(): mutable.Map[BlockNode, mutable.ListBuffer[BlockNode]] =
    val DFMap = mutable.HashMap.empty[BlockNode, mutable.ListBuffer[BlockNode]]
    cfg.blocks.foreach(b => DFMap.put(b, mutable.ListBuffer.empty[BlockNode]))
    cfg.blocks.foreach(b =>
      if b.predecessors.size > 1 then
        b.predecessors.foreach(p =>
          var runner = p
          while (runner != iDomMap.getOrElse(b, throw IllegalStateException("block "+b.toString+" should have an iDOM"))){
            val runnerDF = DFMap.getOrElse(runner, throw IllegalStateException("DF of "+runner.toString+" should have been initialized"))
            runnerDF.addOne(b)
            runner = iDomMap.getOrElse(runner, throw IllegalStateException("block "+runner.toString+" should have an iDOM"))
          }
        )
    )
    DFMap

  val cfg: ControlFlowGraph = ControlFlowGraph(program)

  val nameBlockMap: mutable.Map[String, mutable.Set[BlockNode]] = mutable.HashMap.empty[String, mutable.Set[BlockNode]]

  val globals: mutable.Set[String] = mutable.HashSet.empty[String]

  find_global_vars()
  val iDomMap: mutable.Map[BlockNode, BlockNode] = computeIDomMap()

  val DFMap: mutable.Map[BlockNode, mutable.ListBuffer[BlockNode]] = computeDominatorFrontiers()

}

def findVarsUsed(e: Exp): mutable.Set[String] =
  val varsUsed = mutable.HashSet.empty[String]
  def findVarsUsed_(e: Exp): Unit =
    e match
      case e: BinOp => findVarsUsed_(e.lhs); findVarsUsed_(e.rhs);
      case _: IntLiteral => ;
      case _: StrLiteral => ;
      case e: Var => varsUsed.add(e.name);
      case e: Fn => varsUsed.addAll(findFreeVars(e));
      case e: Rec => findVarsUsed_(e.fn);
      case e: Apply => findVarsUsed_(e.fn); e.args.foreach(findVarsUsed_);
      case e: Build => findVarsUsed_(e.fn); findVarsUsed_(e.size);
      case e: Arr => e.elements.foreach(findVarsUsed_);
      case e: ReadArr => findVarsUsed_(e.array); findVarsUsed_(e.index);
      case UnitE => ;

  findVarsUsed_(e)
  varsUsed

def findFreeVars(e: Fn): mutable.Set[String] =
  val freeVars = mutable.HashSet.empty[String]
  val declaredVars = mutable.HashSet.empty[String]
  declaredVars.addAll(e.params.iterator)
  def findFreeVars_(e: Stmt): Unit =
    e match
      case e: Let => findFreeVars_(e.value); declaredVars.add(e.name);
      case e: Block => e.stmts.foreach(findFreeVars_);
      case e: If => findFreeVars_(e.cond); findFreeVars_(e.bThen); findFreeVars_(e.bElse);
      case e: Return => findFreeVars_(e.value);
      case e: BinOp => findFreeVars_(e.lhs); findFreeVars_(e.rhs);
      case _: StrLiteral => ;
      case _: IntLiteral => ;
      case e: Var => if !declaredVars.contains(e.name) then freeVars.add(e.name);
      case _: Fn => throw IllegalStateException();
      case _: Rec => throw IllegalStateException();
      case e: Apply => findFreeVars_(e.fn); e.args.foreach(findFreeVars_);
      case e: Build => findFreeVars_(e.fn); findFreeVars_(e.size);
      case e: Arr => e.elements.foreach(findFreeVars_);
      case e: ReadArr => findFreeVars_(e.array); findFreeVars_(e.index);
      case UnitE => ;

  findFreeVars_(e.body)
  freeVars