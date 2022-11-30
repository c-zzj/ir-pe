package gen.llvm

object CmpCode extends Enumeration {
  type CmpCode = Value
  val EQ: CmpCode.Value = Value("eq")
  val NE: CmpCode.Value = Value("ne")
  val SGT: CmpCode.Value = Value("sgt")
  val SLT: CmpCode.Value = Value("slt")
  val SGE: CmpCode.Value = Value("sge")
  val SLE: CmpCode.Value = Value("sle")
  
}
