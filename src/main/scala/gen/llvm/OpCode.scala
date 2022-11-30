package gen.llvm

import gen.llvm

import scala.language.implicitConversions

trait OpCode(asmCode: String)

object OpCode {
  case class BinOpCode(asmCode: String) extends OpCode(asmCode)
  val ADD: BinOpCode = BinOpCode("add")
  val SUB: BinOpCode = BinOpCode("sub")
  val MUL: BinOpCode = BinOpCode("mul")
  val SDIV: BinOpCode = BinOpCode("sdiv")
  val SREM: BinOpCode = BinOpCode("srem")
  val SHL: BinOpCode = BinOpCode("shl")
  val LSHR: BinOpCode = BinOpCode("lshr")
  val ASHR: BinOpCode = BinOpCode("ashr")
  val AND: BinOpCode = BinOpCode("and")
  val OR: BinOpCode = BinOpCode("or")

  case class CastOp(asmCode: String) extends OpCode(asmCode)
  val TRUNC: CastOp = CastOp("trunc") // truncation e.g. i64 to i32, i32 to i1
  val ZEXT: CastOp = CastOp("zext") // zero padded extension e.g. i8 to i32
  val SEXT: CastOp = CastOp("sext") // signed extension e.g. i8 to i32
  val PTRTOINT: CastOp = CastOp("ptrtoint") // pointer to int
  val INTTOPTR: CastOp = CastOp("inttoptr") // int to pointer
  val BITCAST: CastOp = CastOp("bitcast") // no-op cast between non-aggregate first class types of the same size


  case class OtherOp(asmCode: String) extends OpCode(asmCode)
  val RET: OtherOp = OtherOp("ret")
  val BR: OtherOp = OtherOp("br")
  val ALLOCA: OtherOp = OtherOp("alloca")
  val LOAD: OtherOp = OtherOp("load")
  val STORE: OtherOp = OtherOp("store")
  val ICMP: OtherOp = OtherOp("icmp") // integer comparison
  val PHI: OtherOp = OtherOp("phi")
  val CALL: OtherOp = OtherOp("call")
  val EXTRACTVALUE: OtherOp = OtherOp("extractvalue")
  val INSERTVALUE: OtherOp = OtherOp("insertvalue")
  val GETELEMENTPTR: OtherOp = OtherOp("getelementptr")
}