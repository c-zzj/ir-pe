package gen.llvm

object Util {
  def join(delimiter: String, elements: Iterable[String]): String =
    var res = ""
    var notFirst = false
    elements.foreach(e => {
      if notFirst then res += delimiter
      res += e
      notFirst = true
    })
    res
}
