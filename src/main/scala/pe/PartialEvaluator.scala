package pe

import ast.*

import scala.collection.mutable

class PartialEvaluator:
  type StaticData = IntLiteral | Arr | Fn | Rec

  case class Node(e: Exp, isEvaluated: Boolean = false)
  def intToBool(int: Int): Boolean = if int == 0 then false else true
  def boolToInt(bool: Boolean): Int = if bool then 1 else 0

  def intAnd(a: Int, b: Int): Int = if a != 0 && b != 0 then b else 0

  def intOr(a: Int, b: Int): Int = if a != 0 then a else if b != 0 then b else 0

  def eval(e: Exp, ctx: mutable.HashMap[String, Exp]): Exp =
    e match
      case BinOp(lhs, op, rhs) =>
        (eval(lhs, ctx), eval(rhs, ctx)) match
          case (IntLiteral(l), IntLiteral(r)) =>
            op match
              case Op.ADD => IntLiteral(l + r)
              case Op.SUB => IntLiteral(l - r)
              case Op.MUL => IntLiteral(l * r)
              case Op.DIV => IntLiteral(l / r)
              case Op.MOD => IntLiteral(l % r)
              case Op.GT => IntLiteral((l >= r).asInstanceOf[Int])
              case Op.LT => IntLiteral((l <= r).asInstanceOf[Int])
              case Op.GE => IntLiteral((l > r).asInstanceOf[Int])
              case Op.LE => IntLiteral((l < r).asInstanceOf[Int])
              case Op.NE => IntLiteral((l != r).asInstanceOf[Int])
              case Op.EQ => IntLiteral((l == r).asInstanceOf[Int])
              case Op.OR => IntLiteral(boolToInt(intToBool(l) || intToBool(r)))
              case Op.AND => IntLiteral(boolToInt(intToBool(l) && intToBool(r)))

      case StrLiteral(str) => Arr(str.map(c => IntLiteral(c.asInstanceOf[Int])).toList)
      case IntLiteral(_) => e
      case Var(name) =>
        val value = ctx.get(name)
        value match
          case Some(data: StaticData) => data
          case Some(_: Exp) => e
          case None => e

      case If(cond, bThen, bElse) =>
        eval(cond, ctx) match
          case IntLiteral(int) => if int != 0 then eval(bThen, ctx) else eval(bElse, ctx)
          case _ => If(cond, eval(bThen, ctx), eval(bElse, ctx)) // this is safe since only functions have side effect, and those ones are not evaluated


      case Fn(args, body, restricted, simplified, hasSideEffect) =>
        if restricted then e
        else if simplified then e
        else Fn(args, eval(body, ctx), restricted, true, hasSideEffect)

      case Rec(name, Fn(args, body, r, s, h)) =>
        val ctx_ =  mutable.HashMap.from(ctx)
        ctx_.put(name, Fn(args, body, true, s, h))
        Rec(name, eval(Fn(args, body, r, s, h), ctx_).asInstanceOf[Fn])

      case Apply(fn, args) =>
        // TODO note that here redundant args are still evaluated
        (eval(fn, ctx), args.map((a: Exp) => eval(a, ctx))) match
          case (Fn(params, body, restricted, s, h), args_) =>
            if restricted then Apply(Fn(params, body, restricted, s, h), args_)
            else
              val ctx_ = mutable.HashMap.from(ctx)
              for (i <- params.indices) if i < args_.length then ctx_.put(params(i), args_(i))
              eval(body, ctx)
          case (fn_, args_) => Apply(fn_, args_)

      case Let(name, e) =>
        ctx.put(name, eval(e, ctx))
        UnitE

      case ExpList(exps) =>
        for (i <- 0 until exps.length-2)
          eval(exps(i), ctx)
        eval(exps.last, ctx)

      case Arr(_) => e

      case Build(fn, size) =>
        (eval(fn, ctx), eval(size, ctx)) match
          case (fn_, IntLiteral(size_)) =>
            Arr(
              List.range(0, size_).map(
                (i: Int) => eval(Apply(fn_, List(IntLiteral(i))), ctx)
              )
            )
          case _ => throw DynamicSizeException(size=size)

      case ReadArr(array, index) =>
        (eval(array, ctx), eval(index, ctx)) match
          case (Arr(elements), IntLiteral(int)) => elements(int)
          case (array_, index_) => ReadArr(array_, index_)

      case UnitE => UnitE

case class DynamicSizeException(private val message: String = "The array size is dynamic.",
                                 private val cause: Throwable = None.orNull,
                                private val size: Exp)
  extends Exception(message, cause):
  def this(size: Exp) =
    this(s"The array size is dynamic. $size", size=size)
