/**
 * (c) Abhijeet Parande
 * Date: 1/27/13
 * Time: 7:10 PM
 */

object beautiful_string extends App{

    val input = scala.io.Source.fromFile(args(0)).getLines().toList

    // validate
    val m : Int = Integer.parseInt(input(0))
    require(m >= 5 && m <= 50)

    for ( i <- 1 to m) {
      val line : String = input(i).toLowerCase()   // case insensitive

      require(line.length >= 2 && line.length <= 500)

      // process the line
      // main logic is to get letter frequency in descending order, and then
      // multiply each frequency by a value = 26 - letter_index_after_re-ordering

      val array = line.filter(_.isLetter) // consider letter only
                    .groupBy(_.toChar)  // group similar chars e.g a -> (a,a) etc
                    .mapValues(_.size)  // map to size (frequency ) a -> 2
                    .toList.sortWith((a,b) => a._2 > b._2) // sort in descending order
                    .toArray

      var value = 0

      for (idx <- 0 until array.length)
           value += array(idx)._2 *(26 - idx)

      println("Case #" + i + ": " + value )


  }

}
