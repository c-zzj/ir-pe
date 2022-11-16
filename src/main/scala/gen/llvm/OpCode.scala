package gen.llvm

import gen.llvm

import scala.language.implicitConversions

trait OpCode(asmCode: String)

object OpCode {
  case class BinOp(asmCode: String) extends OpCode(asmCode)
  val ADD: BinOp = BinOp("add")
  val SUB: BinOp = BinOp("sub")
  val MUL: BinOp = BinOp("mul")
  val SDIV: BinOp = BinOp("sdiv")
  val SHL: BinOp = BinOp("shl")
  val LSHR: BinOp = BinOp("lshr")
  val ASHR: BinOp = BinOp("ashr")
  val AND: BinOp = BinOp("and")
  val OR: BinOp = BinOp("or")

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
}