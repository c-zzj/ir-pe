package gen.llvm

trait LLVMItem:
  def toLL: String

trait LLVMGlobalItem extends LLVMItem

case class Comment(message: String) extends LLVMGlobalItem:
  override def toLL: String = "; " + message

case class FunDef(resultType: LLType,
                  funName: GlobalIdentifier,
                  argList: List[(LLType, LocalIdentifier)],
                  body: List[LLVMItem],
                  prefix: String = "",
                  postfix: String = "") extends LLVMGlobalItem:
  override def toLL: String =
    String.join(" ",
      "define",
      prefix,
      resultType.toLL,
      funName.toLL + "(" + Util.join(", ", argList.map((t, i) => t.toLL + " " + i.toLL)) + ")",
      postfix,
      "{\n"+ body.foldLeft("")((s: String, i: LLVMItem) => s + '\t' + i.toLL + '\n') + "}\n"
    )



case class FunDecl(resultType: LLType,
                   funName: GlobalIdentifier,
                   argList: List[(LLType, String)],
                   prefix: String = "",
                   postfix: String = "") extends LLVMGlobalItem:
  override def toLL: String =
    String.join(" ",
      "declare",
      prefix,
      resultType.toLL,
      funName.toLL + "(" + Util.join(", ", argList.map((t, i) => t.toLL + " " + i)) + ")",
      postfix
    )

case class GlobalVar(varName: GlobalIdentifier, constant: Constant) extends LLVMGlobalItem:
  override def toLL: String = String.join(" ",
    varName.toLL,
    "=",
    "global",
    constant.getType.toLL,
    constant.toLL
  )

