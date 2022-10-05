package ssa

import ssa.*

import scala.collection.mutable

class ControlFlowGraph(program: Stmt) {

  /**
   *
   * @param e the input expression for creating blocks
   * @param cur_block the current block before the analysis
   * @return the current block after analysis of the current expression
   */
  def create_blocks(e: Stmt, cur_block: BlockNode): BlockNode =
    e match
      case e: Let => create_blocks(e.value, cur_block); cur_block.elements.addOne(e); cur_block
      case e: Block =>
        var b = cur_block
        for (stmt <- e.stmts) {
          b = create_blocks(stmt, b)
          stmt match
            case _: Return => return b
            case _ => ;
        }

        cur_block
      case e: If =>
        create_blocks(e.cond, cur_block)
        cur_block.elements.addOne(e)
        val b1 = BlockNode()
        cur_block.successors.addOne(b1)
        b1.predecessors.addOne(cur_block)
        blocks.addOne(b1)
        val b1Last = create_blocks(e.bThen, b1)

        val b2 = BlockNode()
        cur_block.successors.addOne(b2)
        b2.predecessors.addOne(cur_block)
        blocks.addOne(b2)
        val b2Last = create_blocks(e.bElse, b2)

        val after = BlockNode()
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
        after


      case e: Return => create_blocks(e.value, cur_block); cur_block.elements.addOne(e); cur_block
      case e: Rec => create_blocks(e.fn, cur_block)
      case e: Fn =>
        val new_block = FnBlockNode(e)
        blocks.addOne(new_block)
        fun_block_map.put(e, new_block)
        create_blocks(e.body, new_block)
      case e: BinOp => create_blocks(e.lhs, cur_block); create_blocks(e.rhs, cur_block)
      case _: StrLiteral => cur_block
      case _: IntLiteral => cur_block
      case _: Var => cur_block
      case UnitE => cur_block
      case e: Apply => create_blocks(e.fn, cur_block); e.args.foreach(e_ => create_blocks(e_, cur_block)); cur_block
      case e: Build => create_blocks(e.fn, cur_block); create_blocks(e.size, cur_block)
      case e: Arr => e.elements.foreach(e_ => create_blocks(e_, cur_block)); cur_block
      case e: ReadArr => create_blocks(e.array, cur_block); create_blocks(e.index, cur_block)

  val top_level_block: BlockNode = BlockNode()

  val blocks: mutable.ListBuffer[BlockNode] = mutable.ListBuffer.empty[BlockNode]

  val fun_block_map: mutable.Map[Fn, FnBlockNode] = mutable.HashMap.empty[Fn, FnBlockNode]

  blocks.addOne(top_level_block)

  create_blocks(program, top_level_block)
}

class BlockNode(val elements: mutable.ListBuffer[Stmt] = mutable.ListBuffer.empty[Stmt],
                val predecessors: mutable.Set[BlockNode] = mutable.HashSet.empty[BlockNode],
                val successors: mutable.Set[BlockNode] = mutable.HashSet.empty[BlockNode])

class FnBlockNode(val fn: Fn,
                  override val elements: mutable.ListBuffer[Stmt] = mutable.ListBuffer.empty[Stmt],
                  override val predecessors: mutable.Set[BlockNode] = mutable.HashSet.empty[BlockNode],
                  override val successors: mutable.Set[BlockNode] = mutable.HashSet.empty[BlockNode]) extends BlockNode
