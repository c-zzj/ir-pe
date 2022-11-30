package gen

import gen.llvm.*
import conversion.*

import scala.collection.mutable


class ProgramGen(val llvmProg: LLVMProgram):
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
            val id = pInfo.getGlobalId
            pInfo.varIdMap.put(s.name, id)
            globalDataSection.globals.addOne(
              GlobalVar(
                id, CodegenUtil.convertConstant(e)
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
          case _ => throw CodegenUtil.ShouldNotReach("Only constants and functions are allowed for top level assignment")
      case _ => ;
    )

class ProgramInfo(val globalVars: mutable.Set[NameTypePair],
                  var idCounter: Int = 0,
                  val varIdMap: mutable.Map[String, Identifier] = mutable.HashMap.empty[String, Identifier],
                  val blockLabelMap: mutable.Map[Block, Label] = mutable.HashMap.empty[Block, Label]):
  def getGlobalId: GlobalIdentifier =
    idCounter += 1
    GlobalIdentifier(idCounter.toString)

  def getLocalId: LocalIdentifier =
    idCounter += 1
    LocalIdentifier(idCounter.toString)


