package ir
import org.scalatest.flatspec.AnyFlatSpec
import ir.{Stmt, ConversionClosure, PPrint}

class TestConversionClosure extends AnyFlatSpec {
  def test(ir: IR): Unit =
    println("-------BEFORE CONVERSION--------")
    println(PPrint.prettyPrint(ir.code))
    ConversionClosure(ir)
    println("-------AFTER CONVERSION--------")
    println(PPrint.prettyPrint(ir.code))

  "A closure conversion" should "not throw any exception" in {
    /**
     * 1. f
     * 2. a
     * 3. g
     * 4. b
     * 5. h
     * 6. c
     */
    val b1 = Block(
      Assign("f", Fn(List(), Block(
              Assign("a", IntLiteral(0)),
              Assign("g", Fn(List(), Block(
                Assign("b", BinOp(Var("a"), Op.ADD, IntLiteral(1))),
                Assign("h", Fn(List(), Block(
                  Assign("c", BinOp(Var("b"), Op.ADD, Var("a"))),
                  Return(Var("c"))
                ), IRInt(32))),
                Return(Var("h"))
              ), IRInt(32))),
              Return(Apply(Apply(Var("a"), List()), List()))
            ), IRInt(32)))
    )
    val ir1 = IR(b1, 0)
    test(ir1)
  }
}
