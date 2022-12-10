import scala.io.Source
import scala.collection.mutable.ArrayBuffer

val letter_values = Map[String,Int]("a"->1, "b"->2, "c"->3, "d"->4, "e"->5,
  "f"->6, "g"->7, "h"->8, "i"->9, "j"->10, "k"->11, "l"->12, "m"->13, "n"->14,
  "o"->15, "p"->16, "q"->17, "r"->18, "s"->19, "t"->20, "u"->21, "v"->22,
  "w"->23, "x"->24, "y"->25, "z"->26, "A"->27, "B"->28, "C"->29, "D"->30,
  "E"->31, "F"->32, "G"->33, "H"->34, "I"->35, "J"->36, "K"->37, "L"->38,
  "M"->39, "N"->40, "O"->41, "P"->42, "Q"->43, "R"->44, "S"->45, "T"->46,
  "U"->47, "V"->48, "W"->49, "X"->50, "Y"->51, "Z"->52)

// For the first star we need the value of the common article in both
// compartments in the same rucksack
def find_priorities(filename:String):Int= {

  var value: Int = 0

  // Iterate through the lines of the .txt file
  for (line <- Source.fromFile(filename).getLines){
    var first_compartment  = line.slice(0,line.length/2)
    var second_compartment = line.slice(line.length/2, line.length)
    var common_article = first_compartment intersect second_compartment
    var article_value = letter_values.apply(common_article(0).toString)
    value += article_value
  }
  return value
}


// For the second star we need the value of the common article between three
// consecutive rucksacks
def find_priorities_two(filename:String):Int= {

    var value: Int = 0
    var counter: Int = 0
    var rucksacks = ArrayBuffer[String]()

    // Iterate through the lines of the .txt file
    for (line <- Source.fromFile(filename).getLines){
      rucksacks += line
      counter += 1
      if (counter == 3) {
        var common_article = rucksacks(0) intersect rucksacks(1) intersect rucksacks(2)
        var article_value = letter_values.apply(common_article(0).toString)
        value += article_value
        rucksacks.clear
        counter = 0
      }
    }
    return value
}

// First star
println(find_priorities("input.txt"))
// Second star
println(find_priorities_two("input.txt"))
