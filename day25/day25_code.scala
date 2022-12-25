import scala.io.Source
import scala.collection.mutable

val symbols = mutable.Map[Char, Int]('0' -> 0, '1' -> 1, '2' -> 2, '-' -> -1, '=' -> -2)
val symbols_reversed = mutable.Map[Int, Char](0 -> '0', 1 -> '1', 2 -> '2', 3 -> '=', 4 -> '-')

def parse_file(filename:String):Long ={

  var total_sum: Long = 0

  for (line <- Source.fromResource(filename).getLines().filter(_.nonEmpty).toSeq) {
    var integer = 0.toLong
    for ((character, i) <- line.reverse.zipWithIndex) {
      var intermediate: Long = (Math.pow(5, i) * symbols(character)).toLong
      integer += intermediate
    }
    total_sum += integer
  }

  total_sum
}


def through_blizzard (filename: String): String  = {

  var total_sum = parse_file(filename).toDouble

  var result_string = ""

  while (total_sum > 0) {
    val remainder: Int = (total_sum % 5).toInt
    result_string = symbols_reversed(remainder).toString + result_string
    total_sum = Math.floor(total_sum / 5)
    if (remainder > 2) {
      total_sum += 1
    }
  }

  result_string

}

// First and second star
println("")
println(f"Part1 (Test): goal reached in minute = ${through_blizzard(filename = "day25/test.txt")}")
println(f"Part1: goal reached in minute = ${through_blizzard(filename = "day25/input.txt")}")
