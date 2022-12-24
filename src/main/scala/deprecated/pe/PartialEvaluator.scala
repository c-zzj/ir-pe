package deprecated.pe

import deprecated.ast.*
import scala.collection.mutable

class PartialEvaluator:
  type StaticData = IntLiteral | Arr | Fn | Rec

  def intToBool(int: Int): Boolean = if int == 0 then false else true
  def boolToInt(bool: Boolean): Int = if bool then 1 else 0

  def intAnd(a: Int, b: Int): Int = if a != 0 && b != 0 then b else 0

  def intOr(a: Int, b: Int): Int = if a != 0 then a else if b != 0 then b else 0

  def visit(e: Exp): Exp = eval(e, mutable.HashMap.empty)
  def eval(e: Exp, ctx: mutable.HashMap[String, Exp]): Exp =
    e match
      case e: BinOp =>
        (eval(e.lhs, ctx), eval(e.rhs, ctx)) match
          case (lhs: IntLiteral, rhs: IntLiteral) =>
            val l: Int = lhs.int
            val r: Int = rhs.int
            e.op match {
              case Op.ADD => IntLiteral(l + r)
              case Op.SUB => IntLiteral(l - r)
              case Op.MUL => IntLiteral(l * r)
              case Op.DIV => IntLiteral(l / r)
              case Op.MOD => IntLiteral(l % r)
              case Op.GT => IntLiteral(boolToInt(l >= r))
              case Op.LT => IntLiteral(boolToInt(l <= r))
              case Op.GE => IntLiteral(boolToInt(l > r))
              case Op.LE => IntLiteral(boolToInt(l < r))
              case Op.NE => IntLiteral(boolToInt(l != r))
              case Op.EQ => IntLiteral(boolToInt(l == r))
              case Op.OR => IntLiteral(boolToInt(intToBool(l) || intToBool(r)))
              case Op.AND => IntLiteral(boolToInt(intToBool(l) && intToBool(r)))
            }
          case (lhs: ExpList, rhs: ExpList) =>
            val newList = mutable.ListBuffer[Exp]()
            newList.addAll(lhs.exps)
            newList.remove(newList.size - 1)
            newList.addAll(rhs.exps)
            newList.remove(newList.size - 1)
            newList.addOne(eval(BinOp(lhs.exps.last, e.op, rhs.exps.last), ctx))
            ExpList(newList.toList)
          case (lhs: ExpList, rhs: Exp) =>
            val newList = mutable.ListBuffer[Exp]()
            for (i <- lhs.exps.indices) newList.addOne(if i < lhs.exps.size - 1 then lhs.exps(i) else eval(BinOp(lhs.exps(i), e.op, rhs), ctx))
            ExpList(newList.toList)
          case (lhs: Exp, rhs: ExpList) =>
            val newList = mutable.ListBuffer[Exp]()
            for (i <- rhs.exps.indices) newList.addOne(if i < rhs.exps.size - 1 then rhs.exps(i) else eval(BinOp(lhs, e.op, rhs.exps(i)), ctx))
            ExpList(newList.toList)
          case (lhs: Exp, rhs: Exp) => BinOp(lhs, e.op, rhs)

      case e: StrLiteral => Arr(e.str.map(c => IntLiteral(c.asInstanceOf[Int])).toList)
      case e: IntLiteral => e
      case e: Var =>
        val value = ctx.get(e.name)
        value match
          case Some(data: StaticData) => data
          case Some(_: Exp) => e
          case None => e

      case e: If => e.cond match {
        case l: ExpList =>
          val newList = mutable.ListBuffer[Exp]()
          newList.addAll(l.exps)
          newList.remove(newList.size - 1)
          newList.addOne(If(l.exps.last, e.bThen, e.bElse))
          eval(ExpList(newList.toList), ctx)
        case _: Exp =>
          eval(e.cond, ctx) match {
            case i: IntLiteral => if i.int != 0 then eval(e.bThen, ctx) else eval(e.bElse, ctx)
            case _ => If(e.cond, eval(e.bThen, ctx), eval(e.bElse, ctx)) // this is safe since only functions have side effect, and those ones are not evaluated
          }
        }


      case e: Fn =>
        if e.isRestricted || e.isSimplified then e
        else Fn(e.params, eval(e.body, ctx), e.isRestricted, true, e.hasSideEffect)

      case e: Rec =>
        val fn = e.fn
        val ctx_ =  mutable.HashMap.from(ctx)
        ctx_.put(e.name, Fn(fn.params, fn.body, true, fn.isSimplified, fn.hasSideEffect))
        Rec(e.name, eval(fn, ctx_).asInstanceOf[Fn])

      case e: Apply =>
        // TODO note that here redundant args are still evaluated
        (eval(e.fn, ctx), e.args.map((a: Exp) => eval(a, ctx))) match {
          case (fn: Fn, args_) =>
            if fn.isRestricted then Apply(fn, args_)
            else
              val ctx_ = mutable.HashMap.from(ctx)
              for (i <- fn.params.indices) if i < args_.length then ctx_.put(fn.params(i), args_(i))
              eval(fn.body, ctx)

          case (fn, args_) => Apply(fn, args_)
        }

      case e: Let =>
        ctx.put(e.name, eval(e.value, ctx))
        e

      /*
      Evaluate each expression in the list and flatten the list
      */
      case e: ExpList =>
        val flattened = mutable.ListBuffer[Exp]()
        e.exps.map(e_ => eval(e_, ctx)).foreach((ele: Exp) => ele match
          case l : ExpList => flattened.addAll(l.exps)
          case other: Exp => flattened.addOne(other)
        )
        if flattened.size == 1 then flattened(1) else ExpList(flattened.toList)

      case e: Arr => e

      case e: Build =>
        (eval(e.fn, ctx), eval(e.size, ctx)) match {
          case (fn, size_ : IntLiteral) =>
            Arr(
              List.range(0, size_.int).map(
                (i: Int) => eval(Apply(fn, List(IntLiteral(i))), ctx)
              )
            )
          case _ => throw DynamicSizeException(size = e.size)
        }

      case e: ReadArr =>
        (eval(e.array, ctx), eval(e.index, ctx)) match {
          case (array: Arr, index: IntLiteral) => array.elements(index.int)
          case (array, index) => ReadArr(array, index)
        }

      case UnitE => UnitE

case class DynamicSizeException(private val message: String = "The array size is dynamic.",
                                 private val cause: Throwable = None.orNull,
                                private val size: Exp)
  extends Exception(message, cause):
  def this(size: Exp) =
    this(s"The array size is dynamic. $size", size=size)
