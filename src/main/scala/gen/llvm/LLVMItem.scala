package gen.llvm

trait LLVMItem:
  def toLL: String

case class Comment(message: String) extends LLVMItem:
  override def toLL: String = "; " + message

case class FunDef(resultType: Type,
                  funName: GlobalIdentifier,
                  argList: List[(Type, LocalIdentifier)],
                  body: List[Instruction],
                  prefix: String = "",
                  postfix: String = "") extends LLVMItem:
  override def toLL: String =
    String.join(" ",
      "define",
      prefix,
      resultType.toLL,
      funName.toLL + "(" + Util.join(", ", argList.map((t, i) => t.toLL + " " + i.toLL)) + ")",
      postfix,
      "{\n"+ body.foldLeft("")((s: String, i: Instruction) => s + '\t' + i.toLL + '\n') + "}\n"
    )



case class FunDecl(resultType: Type,
                   funName: GlobalIdentifier,
                   argList: List[(Type, LocalIdentifier)],
                   prefix: String = "",
                   postfix: String = "") extends LLVMItem:
  override def toLL: String =
    String.join(" ",
      "declare",
      prefix,
      resultType.toLL,
      funName.toLL + "(" + Util.join(", ", argList.map((t, i) => t.toLL + " " + i.toLL)) + ")",
      postfix
    )

case class GlobalVar(varName: GlobalIdentifier, constant: Constant) extends LLVMItem:
  override def toLL: String = String.join(" ",
    varName.toLL,
    "=",
    "global",
    constant.getType.toLL,
    constant.toLL
  )

