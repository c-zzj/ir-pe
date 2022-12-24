package gen

import ir.IR
import llvm._

import java.io.{File, PrintWriter}
object CodeGen {
  def emitProgram(ir: IR, outputFile: File): Unit =
    val llvmProg = LLVMProgram()
    val progGen = ProgramGen(llvmProg)
    progGen.gen(ir)
    val writer = new PrintWriter(outputFile)
    llvmProg.print(writer)
    writer.close()


}