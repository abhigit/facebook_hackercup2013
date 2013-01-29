import java.lang.String

/**
 * (c) Abhijeet Parande
 * Date: 1/27/13
 * Time: 8:12 PM
 */

object balanced_paren extends App {

  val input = scala.io.Source.fromFile(args(0)).getLines().toList

  val m: Int = Integer.parseInt(input(0))
  require(m>=1 && m<= 50)
  for (i <- 1 to m) {
    val line: String = input(i)
    val len = line.length
    require(len>=1 && len <= 100)
    println("Case #" + (i ) + ": " + (if (balanced(line)) "YES" else "NO"))

  }

  def balanced(str: String): Boolean = {
    def prune(str: List[Char]): List[Char] = {
      def f(cc: List[Char], out: List[Char]): List[Char] = {
        cc match {
          case Nil => out
          case ':' :: tail => f(tail, ':' :: out)
          case '(' :: tail => f(tail, '(' :: out)
          case ')' :: tail => f(tail, ')' :: out)
          case a :: tail => f(tail, out)
        }
      }
      val ret = f(str, List()).reverse
      ret

    }
    def balanced1(str: List[Char]): Boolean = {
      str match {
        case Nil => true
        case '(' :: tail => if (!tail.isEmpty && tail.last == ')')
          balanced1(tail.init)
        else false
        case ':' :: ')' :: tail => balanced1(tail)
        case ':' :: '(' :: tail => balanced1(tail)
        case ':' :: tail => balanced1(tail)
        case _ => false
      }
    }
    balanced1(prune(str.toList))
  }
}
