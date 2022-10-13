package ssa

import scala.collection.mutable

class ConversionSSA(program: Block) {
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

  private def computeIDomMaps(): (mutable.Map[BlockNode, BlockNode], mutable.Map[BlockNode, mutable.ArrayDeque[BlockNode]]) =
    val iDomMap: mutable.Map[BlockNode, BlockNode] = mutable.HashMap.empty[BlockNode, BlockNode]
    val iDomReverseMap = mutable.HashMap.empty[BlockNode, mutable.ArrayDeque[BlockNode]]
    cfg.blocks.foreach(b => iDomReverseMap.put(b, mutable.ArrayDeque.empty[BlockNode]))

    // map from each block and its dominators, ordered from furthest to nearest
    val iDomMap_ = mutable.HashMap.empty[BlockNode, mutable.ArrayDeque[BlockNode]]

    def traverse(cur: BlockNode): Unit =
      val curAncestors = iDomMap_.getOrElseUpdate(cur, mutable.ArrayDeque.empty[BlockNode])
      val curAncestorSet = mutable.HashSet.from(curAncestors)
      for (child <- cur.successors) {
        val childAncestors = iDomMap_.getOrElseUpdate(child, mutable.ArrayDeque.empty[BlockNode])
        if childAncestors.isEmpty then
          childAncestors.addAll(curAncestors).addOne(cur)
        else
          for (i <- childAncestors.size - 1 to 0 by -1) {
            if !curAncestorSet.contains(childAncestors(i)) then childAncestors.remove(i)
          }

        traverse(child)
      }

    roots.foreach(traverse)
    iDomMap_.foreach((b, dominators) => {
      iDomMap.put(b, dominators.last)
      val children = iDomReverseMap.getOrElseUpdate(dominators.last, throw IllegalStateException())
      children.addOne(b)
    })
    (iDomMap, iDomReverseMap)

  private def computeDominatorFrontiers(): mutable.Map[BlockNode, mutable.ArrayDeque[BlockNode]] =
    val DFMap = mutable.HashMap.empty[BlockNode, mutable.ArrayDeque[BlockNode]]
    cfg.blocks.foreach(b => DFMap.put(b, mutable.ArrayDeque.empty[BlockNode]))
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

  private def insertPhiFunctions(): Unit =
    globals.foreach(x =>
      val workList = mutable.HashSet.empty[BlockNode]
      this.nameBlockMap.get(x) match
        case Some(blocks) => workList.addAll(blocks)
        case None =>;
      while (workList.nonEmpty) {
        val b = workList.head
        workList.remove(b)
        for (d <- DFMap.getOrElse(b, mutable.ArrayDeque.empty[BlockNode])){
          // find the content corresponding to d's block, and then insert phi node
          val namePhiMap = blockNodePhiMap.getOrElseUpdate(d, mutable.HashMap.empty[String, Phi])
          namePhiMap.get(x) match
            case Some(phi: Phi) => phi.from.put(b.block, x)
            case None =>
              val phi = Phi()
              phi.from.put(b.block, x)
              namePhiMap.put(x, phi)
              cfg.blockContentMap.getOrElseUpdate(d.block, LinkedSet[Stmt]()).insertAfter(d.prevIf, Let(x, phi))

          workList.add(d)
        }
      }
    )

    // replace the contents of each block
    for ((block, linkedSet) <- cfg.blockContentMap) {
      block.stmts = linkedSet.toList
    }

  private def rename(): Unit =
    val nameCounter = mutable.HashMap.empty[String, Integer]
    val nameStack = mutable.HashMap.empty[String, mutable.Stack[Integer]]

    def newName(n: String): String =
      val i = nameCounter.getOrElseUpdate(n, 1)
      nameCounter.put(n, i+1)
      val name = n + "_" + i
      nameStack.getOrElseUpdate(n, mutable.Stack.empty[Integer]).push(i)
      name

    def curName(n: String): String =
      nameStack.get(n) match {
        case None => n + "_" + 0;
        case Some(stack: mutable.Stack[Integer]) => n + "_" + stack.top;
      }

    def rewrite(e: Exp): Unit =
      e match
        case _: Rec => ;
        case _: Fn => ;
        case e: BinOp => rewrite(e.lhs); rewrite(e.rhs)
        case _: StrLiteral => ;
        case _: IntLiteral => ;
        case e: Var => e.name = curName(e.name)
        case UnitE => ;
        case e: Apply => rewrite(e.fn); e.args.foreach(e_ => rewrite(e_))
        case e: Build => rewrite(e.fn); rewrite(e.size);
        case e: Arr => e.elements.foreach(e_ => rewrite(e_))
        case e: ReadArr => rewrite(e.array); rewrite(e.index)
        case _: Phi => ;

    def rename(blockNode: BlockNode): Unit =
      blockNode.elements.foreach((s: Stmt) =>
        s match
          case s: Let =>
            rewrite(s.value);
            s.name = newName(s.name)
          case s: If => rewrite(s.cond)
          case s: Return => rewrite(s.value)
          case s: Exp => rewrite(s)
      )

      // fill in phi-function parameters
      for (successor <- blockNode.successors){
        val phiFunctions = blockNodePhiMap.getOrElse(successor, mutable.HashMap.empty[String, Phi])
        for ((name, phi) <- phiFunctions){
          if phi.from.contains(blockNode.block) then
            phi.from.put(blockNode.block, curName(name))
        }
      }

      // rename each successor in the dominator tree
      iDomReverseMap.getOrElse(blockNode, mutable.ArrayDeque.empty[BlockNode]).foreach(rename)

      // pop out new name
      blockNode.elements.foreach((s: Stmt) =>
        s match
          case s: Let =>
            nameStack.get(s.name) match {
              case None => ;
              case Some(stack: mutable.Stack[Integer]) => stack.pop()
            }
          case _: If => ;
          case _: Return => ;
          case _: Exp => ;
      )

    roots.foreach(rename)

  val cfg: ControlFlowGraph = ControlFlowGraph(program)

  val nameBlockMap: mutable.Map[String, mutable.Set[BlockNode]] = mutable.HashMap.empty[String, mutable.Set[BlockNode]]

  val globals: mutable.Set[String] = mutable.HashSet.empty[String]

  val roots: mutable.ArrayDeque[BlockNode] = mutable.ArrayDeque.empty[BlockNode]
  roots.addOne(cfg.topLevelBlock)
  cfg.funBlockMap.values.foreach(roots.addOne)

  find_global_vars()

  // iDomMap: map of each block node and its immediate dominator
  // iDomReverseMap: map of each block node and blocks that have this block as their immediate dominator
  val (iDomMap: mutable.Map[BlockNode, BlockNode],
  iDomReverseMap: mutable.Map[BlockNode, mutable.ArrayDeque[BlockNode]]) = computeIDomMaps()

  // map of each block node and its dominator frontiers
  val DFMap: mutable.Map[BlockNode, mutable.ArrayDeque[BlockNode]] = computeDominatorFrontiers()

  // store the unique phi functions inserted to each block node
  private val blockNodePhiMap = mutable.HashMap.empty[BlockNode, mutable.HashMap[String, Phi]]
  insertPhiFunctions()

  rename()
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