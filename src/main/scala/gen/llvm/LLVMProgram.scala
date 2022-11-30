package gen.llvm

import java.io.PrintWriter
import scala.collection.mutable

class LLVMProgram(val sections: mutable.ListBuffer[Section] = mutable.ListBuffer.empty[Section]) {
  def print(writer: PrintWriter): Unit = sections.foreach(sec => sec.print(writer))
}

class Section(val globals: mutable.ListBuffer[LLVMGlobalItem] = mutable.ListBuffer.empty[LLVMGlobalItem]):
  def print(writer: PrintWriter): Unit = globals.foreach(item => writer.println(item.toLL))