package ssa
import org.scalatest.flatspec.AnyFlatSpec
import ssa.{Stmt, ConversionSSA}
import util.PPrint

class TestStmt extends AnyFlatSpec{
  "A Stmt" should "be printed like this" in {
    val s = Block(
      List(
        Let("x", IntLiteral(1)),
        If(
          IntLiteral(1),
          Block(
            List(Let("x", IntLiteral(2)))
          ),
          Block(
            List(Let("x", IntLiteral(3)))
          )
        ),
        Let("x", Var("x"))
      )
    )
    println("-------BEFORE CONVERSION--------")
    println(PPrint.prettyPrint(s))
    ConversionSSA(s)
    println("-------AFTER CONVERSION--------")
    println(PPrint.prettyPrint(s))
  }
}
