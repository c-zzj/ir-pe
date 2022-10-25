package conversion

import scala.collection.mutable

class ControlFlowGraph(program: Block) {

  /**
   * Extracts any Fn in an expression and apply f to them
   * @param e any expression
   * @param f Fn handler
   */
  private def applyToFn(e: Exp, f: Fn => Unit): Unit =
    e match
      case e: Rec => applyToFn(e.fn, f)
      case e: Fn => f(e)
      case e: BinOp => applyToFn(e.lhs, f); applyToFn(e.rhs, f)
      case _: StrLiteral => ;
      case _: IntLiteral => ;
      case _: Var => ;
      case UnitE => ;
      case e: Apply => applyToFn(e.fn, f); e.args.foreach(e_ => applyToFn(e_, f))
      case e: Build => applyToFn(e.fn, f); applyToFn(e.size, f)
      case e: Arr => e.elements.foreach(e_ => applyToFn(e_, f))
      case e: ReadArr => applyToFn(e.array, f); applyToFn(e.index, f)


  /**
   * Construct the CFG
   * @param e the input expression for creating blocks
   * @param curBlockNode the current block before the analysis
   * @return the current block after analysis of the current expression
   */
  def createBlocks(e: Stmt, curBlockNode: BlockNode): BlockNode =
    val createBlockForFn: Fn => Unit = (fn: Fn) => {
      val new_block = FnBlockNode(fn.body, fn)
      blocks.add(new_block)
      funBlockMap.put(fn, new_block)
      createBlocks(fn.body, new_block)
    }

    e match
      case e: Assign =>
        applyToFn(e.value, createBlockForFn)
        curBlockNode.elements.addOne(e)
        curBlockNode
      case e: Block =>
        var b = curBlockNode
        for (stmt <- e.stmts) {
          b = createBlocks(stmt, b)
          stmt match
            case _: Return => return b
            case _ => ;
        }
        b
      case e: If =>
        // create 3 new BlockNodes: 2 for each branch, and 1 for the statements after this If statement

        applyToFn(e.cond, createBlockForFn)

        curBlockNode.elements.addOne(e)

        val b1 = BlockNode(e.bThen)
        curBlockNode.successors.addOne(b1)
        b1.predecessors.addOne(curBlockNode)
        blocks.add(b1)
        val b1Last = createBlocks(e.bThen, b1)

        val b2 = BlockNode(e.bElse)
        curBlockNode.successors.addOne(b2)
        b2.predecessors.addOne(curBlockNode)
        blocks.add(b2)
        val b2Last = createBlocks(e.bElse, b2)

        val after = BlockNode(curBlockNode.block)
        blocks.add(after)

        b1Last.successors.addOne(after)
        after.predecessors.addOne(b1Last)

        b2Last.successors.addOne(after)
        after.predecessors.addOne(b2Last)

        after.prevIf = Some(e)
        after

      case e: Return =>
        applyToFn(e.value, createBlockForFn)
        curBlockNode.elements.addOne(e)
        curBlockNode
      case e: Exp =>
        applyToFn(e, createBlockForFn)
        curBlockNode.elements.addOne(e)
        curBlockNode

  /**
   * The first block of the program
   */
  val topLevelBlock: BlockNode = BlockNode(program)

  /**
   * All CFG blocks of the program
   * Since there are no while loops, the blocks form a rooted directed forest 
   * (each component of the forest is a rooted directed tree)
   */
  val blocks: LinkedSet[BlockNode] = LinkedSet[BlockNode]()

  /**
   * A map from each function to the first BlockNode of the function body
   */
  val funBlockMap: mutable.Map[Fn, FnBlockNode] = mutable.HashMap.empty[Fn, FnBlockNode]

  blocks.add(topLevelBlock)

  createBlocks(program, topLevelBlock)
}

/**
 * A CFG block
 * @param block The closest parent Block Stmt of this CFG block
 * @param elements The elements of this CFG block
 * @param predecessors The predecessors of this CFG block
 * @param successors The successors of this CFG block
 * @param prevIf The If Stmt before this CFG block, if exists. The If Stmt should exist for any
 *               CFG block that has more than 1 predecessor (i.e. the join point of >1 blocks).
 */
class BlockNode(val block: Block,
                val elements: mutable.ArrayDeque[Stmt] = mutable.ArrayDeque.empty[Stmt],
                val predecessors: mutable.Set[BlockNode] = mutable.HashSet.empty[BlockNode],
                val successors: mutable.Set[BlockNode] = mutable.HashSet.empty[BlockNode],
                var prevIf: Option[If] = None)

/**
 * A CFG block of a function body
 * @param block The closest parent Block Stmt of this CFG block
 * @param fn The Fn of which this CFG block is in the body
 */
class FnBlockNode(block: Block, val fn: Fn) extends BlockNode(block)
