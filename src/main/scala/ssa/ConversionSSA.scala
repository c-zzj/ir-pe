package ssa

import scala.collection.mutable
import ssa.PPrint

class ConversionSSA(program: Block) {
  /**
   * Find the global variables in the CFG and store the blocks in which they are defined.
   * A global variable is one that is used in multiple CFG blocks
   * A variable is global iff. it is a free variable in some CFG block.
   */
  private def find_global_vars(): Unit =
    for (block <- cfg.blocks) {
      // function parameters are defined in the first block of the function body
      block match {
        case block: FnBlockNode =>
          if block == cfg.funBlockMap(block.fn) then block.fn.params.foreach((name: String) =>
            val var_blocks = nameBlockMap.getOrElseUpdate(name, mutable.HashSet.empty[BlockNode]);
            var_blocks.add(block)
          );
        case _ => ;
      }

      val varkill = mutable.HashSet.empty[String]
      for (stmt <- block.elements){
        val addGlobal = (name: String) => if ! varkill.contains(name) then globals.add(name)
        stmt match
          case stmt: Assign =>
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

  /**
   * Compute the dominator tree of the CFG
   * A dominator tree has the same vertices as the CFG
   * Two nodes form an edge in the dominator tree if one is the immediate (closest) dominator of the other
   * @return IDomMap: A map from each vertex to its immediate dominator
   *         IDomReverseMap: The successors, ordered from closest to furthest, of each node in the dominator tree.
   */
  private def computeIDomMaps(): (mutable.Map[BlockNode, BlockNode], mutable.Map[BlockNode, LinkedSet[BlockNode]]) =
    val iDomMap: mutable.Map[BlockNode, BlockNode] = mutable.HashMap.empty[BlockNode, BlockNode]
    val iDomReverseMap = mutable.HashMap.empty[BlockNode, LinkedSet[BlockNode]]

    // map from each block and its dominators, ordered from furthest to nearest
    val iDomMap_ = mutable.HashMap.empty[BlockNode, mutable.ArrayDeque[BlockNode]]

    // computes iDomMap_ using DFS traversal of the CFG
    // when traversing an unvisited child, child.ancestors <- cur.ancestors + {cur}
    // when traversing a visited child, child.ancestors <- child.ancestors intersect cur.ancestors
    // by construction, the ancestors are ordered from furthest (root) to closest
    def computeIDom(cur: BlockNode): Unit =
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

        computeIDom(child)
      }

    // computes the successors of each node in the dominator tree
    // performs a BFS traversal that adds each vertex only once
    // at each node, if it has a predecessor, adds this node to the successor list of its predecessor
    // BFS ensures that the successors of each node are ordered from closest to furthest to it.
    def computeIDomReverse(cur: BlockNode): Unit =
      val nodesAdded = mutable.HashSet.empty[BlockNode]
      val blockNodeQueue = mutable.Queue[BlockNode]()
      blockNodeQueue.addOne(cur)
      nodesAdded.add(cur)
      while (blockNodeQueue.nonEmpty) {
        val node = blockNodeQueue.removeHead()
        iDomMap.get(node) match
          case None => ;
          case Some(predecessor: BlockNode) => iDomReverseMap.getOrElseUpdate(predecessor, LinkedSet[BlockNode]()).add(node)

        blockNodeQueue.addAll(node.successors.diff(nodesAdded))
        nodesAdded.addAll(node.successors)
      }

    roots.foreach(computeIDom)
    // the last ancestor is the immediate dominator
    iDomMap_.foreach((b: BlockNode, dominators: mutable.ArrayDeque[BlockNode]) => {
      if dominators.nonEmpty then
        iDomMap.put(b, dominators.last)
    })

    roots.foreach(computeIDomReverse)
    (iDomMap, iDomReverseMap)

  /**
   * Compute the dominator frontier of each node from the dominator tree
   *
   * - Only join points (> 1 predecessor) can be contained in any dominator frontier
   * - A join point is in the dominator frontier of its immediate predecessors
   * - If j in DF(k), l dominates k, and l does not dominate j, then j in DF(j)
   *
   * @return A map from each node to its dominator frontier
   */
  private def computeDominatorFrontier(): mutable.Map[BlockNode, mutable.ArrayDeque[BlockNode]] =
    val DFMap = mutable.HashMap.empty[BlockNode, mutable.ArrayDeque[BlockNode]]
    cfg.blocks.foreach(j =>
      if j.predecessors.size > 1 then
        j.predecessors.foreach(k =>
          var runner = k
          while (runner != iDomMap.getOrElse(j, throw IllegalStateException("block "+j.toString+" should have an iDOM"))){
            val runnerDF = DFMap.getOrElseUpdate(runner, mutable.ArrayDeque.empty[BlockNode])
            runnerDF.addOne(j)
            runner = iDomMap.getOrElse(runner, throw IllegalStateException("block "+runner.toString+" should have an iDOM"))
          }
        )
    )
    DFMap

  /**
   * Insert Phi functions for each global variable
   * For each global variable x, insert a Phi function for it at the beginning of each node in the dominator frontier of each block
   * in which x is defined.
   * Also, if a Phi function for x is inserted in a node, a Phi function will be inserted to each node in the node's dominator frontier
   */
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
              val assignPhi = Assign(x, phi)
              // insert to the LinkedSet of the contents of the Block Stmt
              cfg.blockContentMap.getOrElseUpdate(d.block, LinkedSet[Stmt]()).insertAfter(d.prevIf.get, assignPhi)
              // insert to the CFG block
              d.elements.prepend(assignPhi)
          workList.add(d)
        }
      }
    )

    // update the contents of each Block Stmt
    for ((block, linkedSet) <- cfg.blockContentMap) {
      block.stmts = linkedSet.toList
    }

  /**
   * Rename variables in the CFG such that each variable is assigned exactly once
   */
  private def rename(): Unit =
    val nameCounter = mutable.HashMap.empty[String, Integer]
    val indexStack = mutable.HashMap.empty[String, mutable.Stack[Integer]]

    // update the counter and index stack and return the new name for a given variable name
    def newName(n: String): String =
      val i = nameCounter.getOrElseUpdate(n, 0)
      nameCounter.put(n, i+1)
      val name = n + "_" + i
      indexStack.getOrElseUpdate(n, mutable.Stack.empty[Integer]).push(i)
      name

    // get the current name of the original name of a variable
    def curName(n: String): String =
      indexStack.get(n) match {
        case None => n + "_" + 0;
        case Some(stack: mutable.Stack[Integer]) => n + "_" + stack.top;
      }

    // replace the old names with new names in the expression
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

    /**
     * 1. get new name for each assigned variable
     * 2. replace old names with new names for each expression
     * 3. fill in Phi function parameters for each successor
     * 4. rename each successor in the dominator tree
     * 5. pop out indexes for each assigned variable
     * @param blockNode A root of the CFG forest
     */
    def rename(blockNode: BlockNode): Unit =
      // rename function parameters in the first block of the function body
      blockNode match
        case blockNode: FnBlockNode =>
          if blockNode == cfg.funBlockMap(blockNode.fn) then blockNode.fn.params = blockNode.fn.params.map(newName)
        case _ => ;

      blockNode.elements.foreach((s: Stmt) =>
        s match
          case s: Assign =>
            rewrite(s.value);
            s.name = newName(s.name);
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
      iDomReverseMap.getOrElse(blockNode, LinkedSet[BlockNode]()).foreach(rename)

      // pop out new name
      blockNode.elements.foreach((s: Stmt) =>
        s match
          case s: Assign =>
            indexStack.get(s.name) match {
              case None => ;//throw IllegalStateException("Internal Error: name " + s.name + " should have a counter stack!");
              case Some(stack: mutable.Stack[Integer]) => stack.pop()
            }
          case _: If => ;
          case _: Return => ;
          case _: Exp => ;
      )

    roots.foreach(rename)

  val cfg: ControlFlowGraph = ControlFlowGraph(program)

  /**
   * A map from each variable name to blocks in which it is defined
   */
  val nameBlockMap: mutable.Map[String, mutable.Set[BlockNode]] = mutable.HashMap.empty[String, mutable.Set[BlockNode]]

  val globals: mutable.Set[String] = mutable.HashSet.empty[String]

  val roots: mutable.ArrayDeque[BlockNode] = mutable.ArrayDeque.empty[BlockNode]
  roots.addOne(cfg.topLevelBlock)
  cfg.funBlockMap.values.foreach(roots.addOne)

  find_global_vars()

  // iDomMap: map of each block node and its immediate dominator
  // iDomReverseMap: map of each block node and blocks that have this block as their immediate dominator
  val (iDomMap: mutable.Map[BlockNode, BlockNode],
  iDomReverseMap: mutable.Map[BlockNode, LinkedSet[BlockNode]]) = computeIDomMaps()

  // map of each block node and its dominator frontiers
  val DFMap: mutable.Map[BlockNode, mutable.ArrayDeque[BlockNode]] = computeDominatorFrontier()

  // store the unique phi functions inserted to each block node
  private val blockNodePhiMap = mutable.HashMap.empty[BlockNode, mutable.HashMap[String, Phi]]
  insertPhiFunctions()
  rename()
}

/**
 * Find the used variables in an expression.
 * They are the union of {name: Var(name) in e} and findFreeVars(fn) for each fn: Fn in e.
 * @param e Any expression
 * @return The set of variables names that are used in e
 */
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

/**
 * Find the free variables in a Fn expression
 * @param e any Fn expression
 * @return set of free variables in e
 */
def findFreeVars(e: Fn): mutable.Set[String] =
  val freeVars = mutable.HashSet.empty[String]
  val declaredVars = mutable.HashSet.empty[String]
  declaredVars.addAll(e.params.iterator)
  def findFreeVars_(e: Stmt): Unit =
    e match
      case e: Assign => findFreeVars_(e.value); declaredVars.add(e.name);
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