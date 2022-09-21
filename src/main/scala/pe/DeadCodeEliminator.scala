package pe

import ast.*

import scala.collection.mutable

class DeadCodeEliminator:
  // post order traversal
  val liveExps: mutable.HashSet[Exp] =  mutable.HashSet.empty[Exp]
  val liveVar: mutable.HashSet[String] =  mutable.HashSet.empty[String]
  case class Exp_(e: Exp | List[Exp_], isLive: Boolean)

  def visit(e: Exp): Exp =
    mark_forward(e)
    mark_backward(e)
    sweep(e)

  def mark_backward(e: Exp): Unit =
    val numLiveBefore = liveExps.size
    mark_let(e)
    mark_if(e)
    val numLiveAfter = liveExps.size
    if numLiveBefore != numLiveAfter then mark_backward(e)

  def mark_forward(e: Exp): Unit =
    e match
      case e: BinOp => liveExps.add(e); mark_forward(e.lhs); mark_forward(e.rhs)

      case e: StrLiteral => liveExps.add(e);
      case e: IntLiteral => liveExps.add(e);

      case e: Var => liveExps.add(e); liveVar.add(e.name);

      case e: If => liveExps.add(e); mark_forward(e.cond); mark_forward(e.bThen); mark_forward(e.bElse);

      case e: Fn => liveExps.add(e); mark_forward(e.body)

      case e: Rec => liveExps.add(e); mark_forward(e.fn)

      case e: Apply => liveExps.add(e); mark_forward(e.fn); e.args.foreach(mark_forward);

      case e: Let => if liveVar.contains(e.name) then {
        liveExps.add(e); mark_forward(e.value)
      }

      case e: ExpList => liveExps.add(e); mark_forward(e.exps.last);

      case e: Arr => liveExps.add(e); e.elements.foreach(mark_forward);

      case e: Build => liveExps.add(e); mark_forward(e.fn); mark_forward(e.size);

      case e: ReadArr => liveExps.add(e); mark_forward(e.array); mark_forward(e.index);

      case UnitE => ;
  def mark_let(e: Exp): Unit =
    e match
      case e: BinOp => mark_let(e.lhs); mark_let(e.rhs)

      case e: StrLiteral => ;
      case e: IntLiteral => ;

      case e: Var => ;;

      case e: If => mark_let(e.cond); mark_let(e.bThen); mark_let(e.bElse);

      case e: Fn => mark_let(e.body)

      case e: Rec => mark_let(e.fn)

      case e: Apply => mark_let(e.fn); e.args.foreach(mark_let);

      case e: Let => if liveVar.contains(e.name) then {
        liveExps.add(e)
        if (! liveExps.contains(e.value))
          mark_forward(e.value)
      }

      case e: ExpList => e.exps.foreach(mark_let);

      case e: Arr => e.elements.foreach(mark_let);

      case e: Build => mark_let(e.fn); mark_let(e.size);

      case e: ReadArr => mark_let(e.array); mark_let(e.index);

      case UnitE => ;

  def mark_if(e: Exp): Boolean =
    e match
      case e: BinOp => if liveExps.contains(e) then true else mark_if(e.lhs) || mark_if(e.rhs)

      case e: StrLiteral => false
      case e: IntLiteral => false
      case e: Var => liveExps.contains(e)

      case e: If => if liveExps.contains(e) then true else {
        val isLive = mark_if(e.cond) || mark_if(e.bThen) || mark_if(e.bElse)
        if isLive then liveExps.add(e)
        isLive
      }

      case e: Fn => liveExps.contains(e)

      case e: Rec => liveExps.contains(e)

      case e: Apply => if liveExps.contains(e) then true else mark_if(e.fn) || e.args.foldRight(false)((e_, acc) => acc || mark_if(e_))

      case e: Let => if liveExps.contains(e) then true else mark_if(e.value)

      case e: ExpList => if liveExps.contains(e) then true else e.exps.foldRight(false)((e_, acc) => acc || mark_if(e_))

      case e: Arr => if liveExps.contains(e) then true else e.elements.foldRight(false)((e_, acc) => acc || mark_if(e_))

      case e: Build => if liveExps.contains(e) then true else mark_if(e.size) || mark_if(e.fn)

      case e: ReadArr => if liveExps.contains(e) then true else mark_if(e.array) || mark_if(e.index)

      case UnitE => false

  def sweep(e: Exp): Exp =
    e match
      case e: BinOp => e

      case e: StrLiteral => e
      case e: IntLiteral => e
      case e: Var => e

      case e: If => e

      case e: Fn => e

      case e: Rec => e

      case e: Apply => e

      case e: Let => e

      case e: ExpList => e

      case e: Arr => e

      case e: Build => e

      case e: ReadArr => e

      case UnitE => UnitE
