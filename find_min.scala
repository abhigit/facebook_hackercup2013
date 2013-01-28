import collection.mutable.ArrayBuffer
import java.lang.String

/**
 * Abhijeet Parande
 * Date: 1/28/13
 * Time: 9:59 AM
 *
 */
object find_min extends App {

  val input = scala.io.Source.fromFile(args(0)).getLines().toList

  val m: Int = Integer.parseInt(input(0))
  require(m >= 1 && m <= 50)
  for (i <- 1 to m) {
    val nk: Array[String] = input(2 * i - 1).split("\\s+")
    val abcr: Array[String] = input(2 * i).split("\\s+")

    val n = Integer.parseInt(nk(0))
    val k = Integer.parseInt(nk(1))

    val a = Integer.parseInt(abcr(0))
    val b = Integer.parseInt(abcr(1))
    val c = Integer.parseInt(abcr(2))
    val r = Integer.parseInt(abcr(3))

    // need only 2k+1 elements
    val size = if (2 * k + 1 > n) n else 2 * k + 1
    val arr: Array[Int] = new Array[Int](size)
    arr(0) = a

    // firsk k elements
    for (i <- 1 until k) {
      arr(i) = (b * arr(i - 1) + c) % r
    }
    println(arr.mkString(" "))
    val map = (collection.mutable.Map() ++
      (arr.filter(a => a != 0).groupBy[Int](x => x).mapValues(x => x.size))).withDefaultValue(0)
    println(map)
    val v = {for(i <- 1 to k) yield i }.toSet

    for (i <- k until size) {
      var j = 0
      while (map(j) > 0) j += 1

      for( a <- v) if(map(a) <= 0 )

      arr(i) = j
      if(map(arr(i-k)) > 0) map(arr(i - k)) = map(arr(i - k)) - 1
      map(j) =  1
    }
    println(map)
    println(arr.mkString(" "))
    println("Case #" + (i) + ": " + arr.view(k,size)((n-k)%k))
  }

  //  def find_next(size: Int, k: Int, arr: Array[Int]): Int = {
  //    val map = (collection.mutable.Map() ++ (arr.filter(a => a == -1).groupBy[Int](x => x).mapValues(x => x.size))).withDefaultValue(0)
  //
  //    for (i <- k until size) {
  //      var j = 0
  //      while(map(j) > 0) j += 1
  //      arr(i) = j
  //      map(j) =  1
  //      map(arr(i-k)) =map(arr(i-k)) -1
  //    }
  //
  //    0
  //  }

}
