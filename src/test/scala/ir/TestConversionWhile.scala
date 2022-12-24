package ir
import org.scalatest.flatspec.AnyFlatSpec
import ir.{Stmt, ConversionWhile, PPrint}

class TestConversionWhile extends AnyFlatSpec{
  def test(ir: IR): Unit =
    println("-------BEFORE CONVERSION--------")
    println(PPrint.prettyPrint(ir.code))
    ConversionWhile(ir)
    println("-------AFTER CONVERSION--------")
    println(PPrint.prettyPrint(ir.code))


  "A while conversion" should "not throw any exception" in {
    val b1 = Block(
      Assign("a", IntLiteral(1)),
      While(BinOp(Var("a"), Op.LE, IntLiteral(10)),
        Block(
          Assign("a", BinOp(Var("a"), Op.ADD, IntLiteral(1))),
          While(BinOp(Var("a"), Op.LE, IntLiteral(10)),
            Block(Assign("a", BinOp(Var("a"), Op.ADD, IntLiteral(1)))),
            Block()
          )
        ),
        Block()
      )
    )

    test(IR(b1, 0))


  }
}
