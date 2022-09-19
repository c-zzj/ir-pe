import ast._
object Main {
  def main(args: Array[String]): Unit =
    println("hello")

    trait T
    class X(val int: Int) extends T
    class Y(val int: Int) extends T

    val l = List(1)
    val b = List(2,3)
    print(l.::(1))
}