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

    // need only 2k+1 elements as after first k elemnts remaining k+1 just repeat infinitely
    // so we need to get calculated values for only first 2k+1 or n index, whichever is smaller

    val size = if (2 * k + 1 > n) n else 2 * k + 1
    val arr: Array[Int] = new Array[Int](size)
    val map = collection.mutable.Map[Int, Int]().withDefaultValue(0)
    arr(0) = a
    map(a) = 1
    // firsk k elements
    for (i <- 1 until k) {
      arr(i) = (b * arr(i - 1) + c) % r
      map(arr(i)) += 1   // store frequencies
    }

    // to break out of the loop
    object Done extends Exception {}
    // get next k + 1 or size - k values
    var j = 0
    for (i <- k until size) {
      while (map(j) > 0) j += 1
      try {
        for (a <- j to k) {
          if (map(a) <= 0) {
            arr(i) = j
            throw Done
          }
        }
      } catch {
        case Done => None
      }
      map(j) = 1
      map(arr(i - k)) = map(arr(i - k)) - 1
    }
    var idx = k + (n - k - 1) % (k + 1)
    println("Case #" + (i) + ": " + arr(idx))
  }

}
