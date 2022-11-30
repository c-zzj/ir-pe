package gen

import gen.llvm.*
import conversion.*

import scala.collection.mutable


class ProgramGen(val llvmProg: LLVMProgram):
  private def convertConstant(e: Exp): Constant =
    e match
      case e: IntLiteral => IntConstant(e.numBits, e.int)
      case e: StrLiteral => StringConstant(e.str)
      case e: StructArrLiteral =>
        if e.isArr then
          ArrayConstant(e.elements.map(convertConstant))
        else
          StructConstant(e.elements.map(convertConstant))
      case _ => throw ShouldNotReach()

  def gen(program: IR): Unit =
    val globalDataSection = Section()
    llvmProg.sections.addOne(globalDataSection)

    val globalVars = mutable.HashSet.empty[NameTypePair]
    val pInfo = ProgramInfo(globalVars)
    val funGen = FunGen(pInfo)

    program.code.stmts.foreach(stmt => stmt match
      case s: Assign =>
        globalVars.add(NameTypePair(s.name, s.value.eType));
        s.value match
          case e: (IntLiteral | StrLiteral | StructArrLiteral) =>
            globalDataSection.globals.addOne(
              GlobalVar(
                GlobalIdentifier(s.name), convertConstant(e)
              )
            )
          case e: Fn =>
            val fnSection = Section();
            llvmProg.sections.addOne(fnSection);
            funGen.gen(fnSection, e, s.name)
          case e: Rec =>
            val fnSection = Section();
            llvmProg.sections.addOne(fnSection);
            funGen.gen(fnSection, e.fn, s.name)
          case _ => throw ShouldNotReach("Only constants and functions are allowed for top level assignment")
      case _ => ;
    )



class ProgramInfo(val globalVars: mutable.Set[NameTypePair],
                  var localIdCounter: Int = 0,
                  val localVarIdMap: mutable.Map[String, Int] = mutable.HashMap.empty[String, Int])


final case class ShouldNotReach(private val message: String = "",
                                 private val cause: Throwable = None.orNull)
  extends Exception(message, cause)