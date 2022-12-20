import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

class Number(number_value:Long){
  var value: Long = number_value
}


def parse_file(filename:String, key: Int):mutable.ListBuffer[Number] = {
  val numbers: mutable.ListBuffer[Number] = mutable.ListBuffer[Number]()
  val data = Source.fromResource(filename).getLines().filter(_.nonEmpty).toSeq

  for (num <- data){
    val number = new Number(num.toLong * key)
    numbers += number
  }

  numbers
}

def decrypt(filename: String, key: Int, number_of_mix: Int): Long  = {

  val numbers = parse_file(filename, key)
  val final_list = numbers.clone()

  for (i <- Range(0, number_of_mix)){
    for (num <- numbers.zipWithIndex) {
      val old_position = final_list.indexOf(num._1)
      var new_position = ((old_position + num._1.value) % (numbers.length - 1)).toInt
      if (new_position <= 0) {
        new_position = numbers.length - 1 + new_position
      }
      final_list -= num._1
      final_list.insert(new_position, num._1)
    }
  }

  var pos_zero = 0
  for (i <- final_list) {
    if (i.value == 0) {
      pos_zero = final_list.indexOf(i)
    }
  }

  final_list((1000 + pos_zero)%final_list.size).value + final_list((2000 + pos_zero)%final_list.size).value +
    final_list((3000 + pos_zero)%final_list.size).value
}

// First and second star
println(f"Part1 (Test): sum of coordinates = ${decrypt(filename = "day20/test.txt", key = 1, number_of_mix= 1)}")
println(f"Part1: sum of coordinates = ${decrypt(filename = "day20/input.txt", key = 1, number_of_mix= 1)}")
println(f"Part2 (Test): sum of coordinates = ${decrypt(filename = "day20/test.txt", key = 811589153, number_of_mix= 10)}")
println(f"Part2: sum of coordinates = ${decrypt(filename = "day20/input.txt", key = 811589153, number_of_mix= 10)}")
