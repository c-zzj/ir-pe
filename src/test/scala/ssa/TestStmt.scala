package ssa
import org.scalatest.flatspec.AnyFlatSpec
import ssa.Stmt
import util.PPrint

class TestStmt extends AnyFlatSpec{
  "A Stmt" should "be printed like this" in {
    val s = Block(List(Let("x", IntLiteral(3))))
    print(PPrint.prettyPrint(s))
  }
}
