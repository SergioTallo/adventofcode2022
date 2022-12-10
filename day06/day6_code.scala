import scala.io.Source
import scala.collection.mutable.ArrayBuffer


// For the first star we only need to know the elf carrying the most calories
// For the first star we only need to know the elf carrying the most calories
def find_message(filename:String, num_distinct:Int):Int= {
  var letters = ArrayBuffer[Char]()
  var position: Int = 0

  // Iterate through the lines of the .txt file
  for (line <- Source.fromFile(filename).getLines.filter(!_.isEmpty())){
    for (c<-line){
      position += 1
      letters += c
      if (letters.length == num_distinct){
        if (letters.distinct.length == num_distinct){
          return position
        }  else {
          letters = letters.drop(1)
        }
      }
    }
  }
  return 0
}


// First star
println(find_message("input.txt", 4))
// Second star
println(find_message("input.txt", 14))
