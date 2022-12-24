package gen
import org.scalatest.flatspec.AnyFlatSpec
import ir.*
import gen.CodeGen

import java.io.File

class TestCodeGen extends AnyFlatSpec {
  "Code generation" should "not throw any exception" in {
    val path = "src/test/llvm/"
    val output: File = new File(path + "1.ll")

    val printi = Assign("printi", Rec("printi", Fn(
      List(NameTypePair("i",IRInt(32))),
      Block(
        If(
          BinOp(Var("i"), Op.GT, IntLiteral(9, 32)),
          Block(
            Apply(Var("printi"), List(BinOp(Var("i"), Op.DIV, IntLiteral(10, 32))))
          ),
          Block(

          )
        ),
        Assign("c", BinOp(
          ConvertInt(
            BinOp(Var("i"), Op.MOD, IntLiteral(10, 32)), 8),
          Op.ADD,
          IntLiteral(48, 8))),
        Apply(Var("putchar"), List(Var("c"))),
        Return(VoidE)
      ),
      IRVoid
    )))

    val b1 = Block(
      printi,
      Assign("fibonacci",
        Rec("fibonacci", Fn(
          List(NameTypePair("n", IRInt(32))),
          Block(
            If(
              BinOp(
                BinOp(Var("n"), Op.EQ, IntLiteral(1, 32)),
                Op.OR,
                BinOp(Var("n"), Op.EQ, IntLiteral(2, 32)),
              ),
              Block(Return(IntLiteral(1, 32))),
              Block(
                Return(BinOp(
                  Apply(Var("fibonacci"), List(BinOp(Var("n"), Op.SUB, IntLiteral(1, 32)))),
                  Op.ADD,
                  Apply(Var("fibonacci"), List(BinOp(Var("n"), Op.SUB, IntLiteral(2, 32)))),
                ))
              )
            )
          ),
          IRInt(32)))
      ),
      Assign("main",
        Fn(
          List(),
          Block(
            Assign("res", Apply(Var("fibonacci"), List(IntLiteral(11,32)))),
            Apply(
              Var("printi"),
              List(Var("res"))
            ),
            Return(IntLiteral(0))
          ),
          IRInt(32)
        )
      ),
    )

    val ir = IR(b1, 0)
    TypeAnalyzer(ir)
    CodeGen.emitProgram(ir, output)

  }
}
