// This code is not working, i couldn't find any
// solution in scala, therefore, i solved this
// day using python. I didn't store that code
// because this repository should be in scala.


import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.PriorityQueue

def eval_string(str:String, list_things: ListBuffer[String]): ListBuffer[String]  = {
  var list_things2 = list_things
  if (str.startsWith("[") && str.endsWith("]")) {
    var str_ = str.drop(1).dropRight(1)
    println(str_)
    eval_string(str.drop(1).dropRight(1), list_things)
  }
  else {
    println(str(0))
    if (str(0).isDigit){
      println("is digit")
      list_things2 += str(0).toString
      if (str.length > 1) {
        eval_string(str.drop(2), list_things2)
      }
      else {
        return list_things2
      }
    }
  }

  return list_things2
}


def parse_map(filename:String): Int = {

  val data = Source.fromResource(filename).getLines().toSeq

  var right_pairs_sum = 0

  var pair_number = 1
  var count_pair = 1
  var left_string = ""
  var right_string = ""

  for (line <- data) {
    if (line != "") {
      if (count_pair == 1){
        left_string = line
        count_pair += 1
      }
      else if (count_pair == 2){
        right_string = line
        count_pair = 1
      }
    } else {
      var list_things: ListBuffer[String] = ListBuffer()
      eval_string(left_string, list_things)
      println(list_things)
      println(f"pair_number $pair_number")
      pair_number += 1
      println("")
    }
  }

return right_pairs_sum

}

// First and second star
println(parse_map("day13/test.txt"))
//println(search_grid("day12/input.txt", 1))
