import scala.io.Source
import scala.collection.mutable.ArrayBuffer


// For the first star
def move_crates(filename:String):String= {
  var crates = ArrayBuffer[ArrayBuffer[String]]()
  var position: Int = 0
  var job: Int = 0

  // Iterate through the lines of the .txt file
  for (line <- Source.fromFile(filename).getLines.filter(!_.isEmpty())){

    if ((job == 0) && (line contains "move")){
      job = 1
      for (i <- Range(0, crates.length)){
        crates(i) = crates(i).reverse
      }
    }

    if (job == 0){
      if (!(line contains "1")){
        var line2 = line.replaceAll("\\s+$", "").replaceAll("         ", " . . ").replaceAll("     ", " . ")
        line2 = line2.replaceAll("    ", " . ")
        var ar = line2.split(" ")
        for (c <- ar){
          if (crates.length <= position){
            var crate_number = ArrayBuffer[String]()
            crates += crate_number
            if ((c != ".") && (c!= "")){
              crate_number += c
            }
          } else {
            if ((c != ".") && (c!= "")){
              crates(position) += c
            }
          }
          position += 1
        }
        position = 0
      }
    } else {

      var numbers = ("""\d+""".r findAllIn line).toList

      for (_ <- Range(0, numbers(0).toInt)){
        crates(numbers(2).toInt -1) += crates(numbers(1).toInt -1).last
        crates(numbers(1).toInt -1) = crates(numbers(1).toInt -1).dropRight(1)
      }

    }
  }

  var a: String = ""
  for (i <- Range(0, crates.length)){
    a += crates(i).last.replace("[", "").replace("]", "")
  }

  return a
}


// For the second star
def move_crates2(filename:String):String= {
  var crates = ArrayBuffer[ArrayBuffer[String]]()
  var position: Int = 0
  var job: Int = 0

  // Iterate through the lines of the .txt file
  for (line <- Source.fromFile(filename).getLines.filter(!_.isEmpty())){

    if ((job == 0) && (line contains "move")){
      job = 1
      for (i <- Range(0, crates.length)){
        crates(i) = crates(i).reverse
      }
    }

    if (job == 0){
      if (!(line contains "1")){
        var line2 = line.replaceAll("\\s+$", "").replaceAll("         ", " . . ").replaceAll("     ", " . ")
        line2 = line2.replaceAll("    ", " . ")
        var ar = line2.split(" ")
        for (c <- ar){
          if (crates.length <= position){
            var crate_number = ArrayBuffer[String]()
            crates += crate_number
            if ((c != ".") && (c!= "")){
              crate_number += c
            }
          } else {
            if ((c != ".") && (c!= "")){
              crates(position) += c
            }
          }
          position += 1
        }
        position = 0
      }
    } else {

      var numbers = ("""\d+""".r findAllIn line).toList

      var length_crate = crates(numbers(1).toInt -1).length
      var start_position = length_crate - numbers(0).toInt
      var end_position = length_crate

      crates(numbers(2).toInt -1) = crates(numbers(2).toInt -1) ++ crates(numbers(1).toInt -1).slice(start_position, end_position)
      crates(numbers(1).toInt -1) = crates(numbers(1).toInt -1).dropRight(numbers(0).toInt)
    }
  }

  var a: String = ""
  for (i <- Range(0, crates.length)){
    a += crates(i).last.replace("[", "").replace("]", "")
  }

  return a
}


// First star
println(move_crates("input.txt"))
// Second star
println(move_crates2("input.txt"))
