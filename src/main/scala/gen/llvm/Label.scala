package gen.llvm

case class Label(name: String) extends LLVMItem:
  def this() =
    this("__LABEL_" + Label.ctr.toString)
    Label.ctr += 1

  override def toLL: String = name + ":"

  def toIdentifier: LocalIdentifier = LocalIdentifier(name)

case object Label:
  private var ctr: Int = 0

  def apply(): Label = new Label()

