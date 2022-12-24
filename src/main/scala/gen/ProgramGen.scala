package gen

import gen.llvm.*
import ir.*

import scala.collection.mutable


class ProgramGen(val llvmProg: LLVMProgram):
  def addBuiltInFunctions(section: Section): Unit ={
    val builtin = List(
      FunDecl(
        LLType.TVoid, GlobalIdentifier("putchar"), List((LLType.TInt(8), "nocapture")), postfix = "nounwind"
      ),
      FunDecl(
        LLType.TVoid, GlobalIdentifier("puts"), List((LLType.TPtr(LLType.TInt(8)), "nocapture")), postfix = "nounwind"
      ),
      FunDecl(
        LLType.TPtr(LLType.TInt(8)), GlobalIdentifier("itoa"), List((LLType.TInt(32), "nocapture")), postfix = "nounwind"
      ),
      FunDecl(
        LLType.TInt(32), GlobalIdentifier("atoi"), List((LLType.TPtr(LLType.TInt(8)), "nocapture")), postfix = "nounwind"
      )
    )

    section.globals.addAll(builtin)
  }
  def gen(program: IR): Unit =
    val globalDataSection = Section()
    llvmProg.sections.addOne(globalDataSection)
    addBuiltInFunctions(globalDataSection)

    val globalVars = mutable.HashSet.empty[NameTypePair]
    val pInfo = ProgramInfo(globalVars)
    val funGen = FunGen(pInfo)

    program.code.stmts.foreach(stmt => stmt match
      case s: Assign =>
        globalVars.add(NameTypePair(s.name, s.value.eType));
        s.value match
          case e: (IntLiteral | StrLiteral | StructArrLiteral) =>
            val id = pInfo.getGlobalId;
            pInfo.varIdMap.put(s.name, id);
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
    GlobalIdentifier("_" + idCounter.toString)

  def getLocalId: LocalIdentifier =
    idCounter += 1
    LocalIdentifier("_" + idCounter.toString)

  def addBuiltinIds(): Unit =
    varIdMap.addOne("puts", GlobalIdentifier("puts"))
    varIdMap.addOne("putchar", GlobalIdentifier("putchar"))
    varIdMap.addOne("itoa", GlobalIdentifier("itoa"))
    varIdMap.addOne("atoi", GlobalIdentifier("atoi"))

  addBuiltinIds()
