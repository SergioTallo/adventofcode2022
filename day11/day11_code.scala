import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

val letter_values = Map[String,Int]("a"->1, "b"->2, "c"->3, "d"->4, "e"->5,
  "f"->6, "g"->7, "h"->8, "i"->9, "j"->10, "k"->11, "l"->12, "m"->13, "n"->14,
  "o"->15, "p"->16, "q"->17, "r"->18, "s"->19, "t"->20, "u"->21, "v"->22,
  "w"->23, "x"->24, "y"->25, "z"->26)

class Monkey() {
  var number: Int = 0
  var items: ArrayBuffer[Long] = ArrayBuffer[Long]()
  var operation: String = ""
  var test: Int = 0
  var throw_true: Int = 0
  var throw_false: Int = 0
  var inspections: Int = 0
}

def parse_file(filename:String, monkeys:Map[Int, Monkey]): Unit ={

  var monkey_number: Int = 0

  for (line <- Source.fromFile(filename).getLines) {
    if (line.startsWith("Monkey")) {
      val monkey = new Monkey()
      monkey.number = line.trim.split(" ")(1)(0).asDigit
      monkeys(monkey.number) = monkey
      monkey_number = monkey.number
    }

    if (line.startsWith("  Starting items")) {
      for (item <- line.trim.split(":")(1).trim.split(",")) {
        monkeys(monkey_number).items += item.trim.toLong
      }
    }

    if (line.startsWith("  Operation")) {
      monkeys(monkey_number).operation = line.trim.split("=")(1).trim
    }

    if (line.startsWith("  Test")) {
      monkeys(monkey_number).test = line.trim.split("by")(1).trim.toInt
    }

    if (line contains ("If true")) {
      monkeys(monkey_number).throw_true = line.trim.split("monkey")(1).trim.toInt
    }

    if (line contains ("If false")) {
      monkeys(monkey_number).throw_false = line.trim.split("monkey")(1).trim.toInt
    }
  }
}

// Use same function for both stars, the only thing that changes is the rounds and the divisor
def pixels(filename:String, rounds:Int, divisor:Int):Long= {

  val monkeys = Map[Int, Monkey]()

  parse_file(filename, monkeys)

  var supermodulo: Int = 1

  for (i <- Range(0, monkeys.size)) {
    supermodulo = supermodulo * monkeys(i).test
  }

  var temp_item: Long = 0

  for (_ <- Range(0, rounds)){
    for (monkey_number <- Range(0, monkeys.size)){
      for (item <- monkeys(monkey_number).items){
        monkeys(monkey_number).inspections += 1
        temp_item = item
        if (monkeys(monkey_number).operation contains "+"){
          var operator = monkeys(monkey_number).operation.split("\\+")(1).trim.toInt
          temp_item += operator
          if (divisor != 1){
            temp_item = (temp_item / divisor).floor.toLong
          }
        } else {
          val operator = monkeys(monkey_number).operation.split("\\*")(1).trim
          if (operator == "old"){
            temp_item = item * item
          } else {
            temp_item = item * operator.toInt
          }
          if (divisor != 1) {
            temp_item = (temp_item / divisor).floor.toLong
          }

          temp_item = temp_item % supermodulo

        }

        if (temp_item % monkeys(monkey_number).test == 0){
          monkeys(monkeys(monkey_number).throw_true).items += temp_item
        } else {
          monkeys(monkeys(monkey_number).throw_false).items += temp_item
        }
      }
      monkeys(monkey_number).items.clear()
    }
  }

  var max_value:Long = 0
  var second_max_value:Long = 0

  for (i <- Range (0, monkeys.size)) {
    if (monkeys(i).inspections > max_value) {
      second_max_value = max_value
      max_value = monkeys(i).inspections
    } else if (monkeys(i).inspections > second_max_value) {
      second_max_value = monkeys(i).inspections
    }
  }

  max_value * second_max_value
}

// First and second star
println(pixels("day11/input.txt", 20, 3))
println(pixels("day11/input.txt", 10000, 1))
