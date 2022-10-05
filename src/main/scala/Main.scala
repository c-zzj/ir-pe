import ast._
object Main {
  def main(args: Array[String]): Unit =
    println("hello")

    trait T
    class X(val int: Int) extends T
    class Y(val int: Int) extends T

    val l: T = X(3)
    l match
      case a_ :X => print(1)
      case b:Y => print(2)

    for (i <- 10 to 1 by -1){
      print(i)
    }
}