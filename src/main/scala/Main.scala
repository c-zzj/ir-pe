import ast._
object Main {
  def main(args: Array[String]): Unit =
    println("hello")
    val v = "hello"
    println(v.map(c => IntLiteral(c.asInstanceOf[Int])).toList)
    trait a
    case class B(int: Int) extends a
    val a = List(1,2,3)
    println(a.lift(2))
    val list = List.range(0,3)
    println(list.last)
    
}