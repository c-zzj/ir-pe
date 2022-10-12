package ssa

import ssa.*
import ssa.LinkedSet

import scala.collection.mutable

class ControlFlowGraph(program: Block) {

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
      blocks.addOne(new_block)
      funBlockMap.put(fn, new_block)
      createBlocks(fn.body, new_block)
    }

    e match
      case e: Let =>
        applyToFn(e.value, createBlockForFn)
        curBlockNode.elements.addOne(e)
        curBlockNode
      case e: Block =>
        blockContentMap.put(e, LinkedSet[Stmt](e.stmts))
        var b = curBlockNode
        for (stmt <- e.stmts) {
          b = createBlocks(stmt, b)
          stmt match
            case _: Return => return b
            case _ => ;
        }
        curBlockNode
      case e: If =>
        applyToFn(e.cond, createBlockForFn)

        curBlockNode.elements.addOne(e)

        val b1 = BlockNode(e.bThen)
        curBlockNode.successors.addOne(b1)
        b1.predecessors.addOne(curBlockNode)
        blocks.addOne(b1)
        val b1Last = createBlocks(e.bThen, b1)

        val b2 = BlockNode(e.bElse)
        curBlockNode.successors.addOne(b2)
        b2.predecessors.addOne(curBlockNode)
        blocks.addOne(b2)
        val b2Last = createBlocks(e.bElse, b2)

        val after = BlockNode(curBlockNode.block)
        if (b1Last.elements.isEmpty) {
          for (b <- b1Last.predecessors){
            b.successors.remove(b1Last)
            b.successors.addOne(after)
            after.predecessors.addOne(b)
          }
        }
        if (b2Last.elements.isEmpty) {
          for (b <- b2Last.predecessors) {
            b.successors.remove(b1Last)
            b.successors.addOne(after)
            after.predecessors.addOne(b)
          }
        }
        after.prevIf = e
        after

      case e: Return =>
        applyToFn(e.value, createBlockForFn)
        curBlockNode.elements.addOne(e)
        curBlockNode
      case e: Exp =>
        applyToFn(e, createBlockForFn)
        curBlockNode.elements.addOne(e)
        curBlockNode

  // the root block
  val topLevelBlock: BlockNode = BlockNode(program)

  // all blocks
  val blocks: mutable.ArrayDeque[BlockNode] = mutable.ArrayDeque.empty[BlockNode]

  // map of each function and its root block
  val funBlockMap: mutable.Map[Fn, FnBlockNode] = mutable.HashMap.empty[Fn, FnBlockNode]

  // map of each block statement and the linkedset of its content
  val blockContentMap: mutable.Map[Block, LinkedSet[Stmt]] = mutable.HashMap.empty[Block, LinkedSet[Stmt]]

  blocks.addOne(topLevelBlock)

  createBlocks(program, topLevelBlock)
}

class BlockNode(val block: Block,
                val elements: mutable.ArrayDeque[Stmt] = mutable.ArrayDeque.empty[Stmt],
                val predecessors: mutable.Set[BlockNode] = mutable.HashSet.empty[BlockNode],
                val successors: mutable.Set[BlockNode] = mutable.HashSet.empty[BlockNode],
                var prevIf: If = null)

class FnBlockNode(block: Block, val fn: Fn) extends BlockNode(block)
