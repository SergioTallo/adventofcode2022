import scala.io.Source
import scala.collection.mutable.ArrayBuffer

// For the first star
def find_reconsider(filename:String):Int= {
  var reconsider: Int = 0
  // Iterate through the lines of the .txt file
  for (line <- Source.fromFile(filename).getLines){
    var assignment_pair = line.split(",")
    var elf1 = assignment_pair(0).split("-")
    var elf2 = assignment_pair(1).split("-")
    if (elf1(0).toInt >= elf2(0).toInt && elf1(0).toInt <= elf2(1).toInt && elf1(1).toInt <= elf2(1).toInt) {
      reconsider += 1
    } else if (elf2(0).toInt >= elf1(0).toInt && elf2(0).toInt <= elf1(1).toInt && elf2(1).toInt <= elf1(1).toInt){
      reconsider += 1
    }
  }
  return reconsider
}


// For the second star 
def find_reconsider2(filename:String):Int= {
  var reconsider: Int = 0
  // Iterate through the lines of the .txt file
  for (line <- Source.fromFile(filename).getLines){
    var assignment_pair = line.split(",")
    var elf1 = assignment_pair(0).split("-")
    var elf2 = assignment_pair(1).split("-")
    if (elf1(1).toInt >= elf2(0).toInt && elf1(0).toInt <= elf2(0).toInt) {
      reconsider += 1
    } else if (elf2(1).toInt >= elf1(0).toInt && elf2(0).toInt <= elf1(0).toInt){
      reconsider += 1
    }
  }
  return reconsider
}

// First star
println(find_reconsider("input.txt"))
// Second star
println(find_reconsider2("input.txt"))
