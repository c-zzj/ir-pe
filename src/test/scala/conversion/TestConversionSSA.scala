package conversion
import org.scalatest.flatspec.AnyFlatSpec
import conversion.{Stmt, ConversionSSA, PPrint}


class TestConversionSSA extends AnyFlatSpec{
  def test(b: Block): Unit =
    println("-------BEFORE CONVERSION--------")
    println(PPrint.prettyPrint(b))
    ConversionSSA(b)
    println("-------AFTER CONVERSION--------")
    println(PPrint.prettyPrint(b))

  "An SSA Conversion" should "not throw any exception" in {
    val b1 = Block(
      Assign("x", IntLiteral(1)),
      If(
        IntLiteral(1),
        Block(Assign("x", IntLiteral(2))),
        Block(Assign("x", IntLiteral(3)))
      ),
      Assign("x", Var("x"))
    )
    test(b1)

    val b2 = Block(
      Assign("x", IntLiteral(1)),
      Assign("f", Fn(List("p"), Block(
        If(
          IntLiteral(1),
          Block(Assign("p", Var("x"))),
          Block(Assign("p", Var("p")))
        ),
        Return(Var("p"))
      ))),
      Apply(Var("f"), List(IntLiteral(3)))
    )
    test(b2)

    val b3 = Block(
      If(
        IntLiteral(1),
        Block(Assign("x", StrLiteral("c"))),
        Block(If(
          IntLiteral(2),
          Block(Assign("x", StrLiteral("a"))),
          Block(Assign("x", StrLiteral("b")))
        ))
      ),
      Assign("y",Var("x"))
    )
    test(b3)

    val b4 = Block(
      Assign("x", IntLiteral(1)),
      Assign("f", Rec("f", Fn(List(), Block(
        Assign("y", Var("x")),
        Assign("x", Var("x")),
        Apply(Var("f"), List())
      )))),
      Assign("x", IntLiteral(2))
    )

    test(b4)

    println(PPrint.prettyPrint(Block(b4.stmts.toList.appended(Return(VoidE)))))
  }

}
