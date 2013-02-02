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

  def balanced(str : String) : Boolean = {
    var open = 0
    var min =0
    object Break extends Error
    try{
      for (i <- 0 until str.length) {
         if (str(i) == '(') {
           open +=1
           if (i>0 && str(i-1) != ':') {
             min +=1
           }
         }else if (str(i) == ')') {
           min = math.max(0,min-1)
           if (i>0 && str(i-1) !=':') {
              open -= 1
           }
           if (open < 0) {
             throw Break 
           }
         }else {
           if(!(str(i).isLetter || str(i) == ' ' || str(i) == ':')){
              throw Break  
           }
         }
      }
    }catch {
      case Break => open = -1 
    }
     min == 0 && open >=0
  }
}
