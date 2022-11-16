package gen.llvm

class LLVMProgram(val llvmItems: List[LLVMItem]) {

  def toLL: String = llvmItems.foldLeft("")((s: String, item: LLVMItem) => s + '\n' + item.toLL)
}
