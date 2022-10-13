package ssa
import org.scalatest.flatspec.AnyFlatSpec
import ssa.{Stmt, ConversionSSA, PPrint}


class TestConversionSSA extends AnyFlatSpec{
  def test(b: Block): Unit =
    println("-------BEFORE CONVERSION--------")
    println(PPrint.prettyPrint(b))
    ConversionSSA(b)
    println("-------AFTER CONVERSION--------")
    println(PPrint.prettyPrint(b))

  "A SSA Conversion" should "not throw any exception" in {
    val b1 = Block(
      Let("x", IntLiteral(1)),
      If(
        IntLiteral(1),
        Block(Let("x", IntLiteral(2))),
        Block(Let("x", IntLiteral(3)))
      ),
      Let("x", Var("x"))
    )
    test(b1)

    val b2 = Block(
      Let("x", IntLiteral(1)),
      Let("f", Fn(List("p1", "p2"), Block(
        If(
          Var("x"),
          Block(Let("res", Var("p1"))),
          Block(Let("res", Var("p2")))
        ),
        Return(Var("res"))
      ))),
      Apply(Var("f"), List(IntLiteral(3),IntLiteral(4)))
    )
    test(b2)

    val b3 = Block(
      If(
        IntLiteral(1),
        Block(Let("x", StrLiteral("c"))),
        Block(If(
          IntLiteral(2),
          Block(Let("x", StrLiteral("a"))),
          Block(Let("x", StrLiteral("b")))
        ))
      ),
      Let("y",Var("x"))
    )
    test(b3)
  }

}
