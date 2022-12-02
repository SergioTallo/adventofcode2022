import scala.io.Source
import scala.collection.mutable.ArrayBuffer


// For the first star we need to follow the strategy playing what is in the list
def rock_paper_scissors_strategy(filename:String):Int= {
  case class Game(oponent: String, me: String)
  var result = 0
  var lines = 0
  // Iterate through the lines of the .txt file
  for (line <- Source.fromFile(filename).getLines){
    lines += 1
    var line_array = line.split(" ")
    val list_game = new Game(line_array(0), line_array(1))

    list_game match{
      case Game("A", "X") => result += 4 //1 + draw (rock / rock)
      case Game("A", "Y") => result += 8 //2 + win (rock / paper)
      case Game("A", "Z") => result += 3 //3 + lose (rock / scissors)
      case Game("B", "X") => result += 1 //1 + lose (paper / rock)
      case Game("B", "Y") => result += 5 //2 + draw (paper / paper)
      case Game("B", "Z") => result += 9 //3 + win (paper / scissors)
      case Game("C", "X") => result += 7 //1 + win (scissors / rock)
      case Game("C", "Y") => result += 2 //2 + lose (scissors / paper)
      case Game("C", "Z") => result += 6 //3 + draw (scissors / scissors)
    }
  }
  return result
}

// For the second star we need to follow the strategy wining, losing or drawing
def rock_paper_scissors_strategy2(filename:String):Int= {
  case class Game(oponent: String, me: String)
  var result = 0
  var lines = 0
  // Iterate through the lines of the .txt file
  for (line <- Source.fromFile(filename).getLines){
    lines += 1
    var line_array = line.split(" ")
    val list_game = new Game(line_array(0), line_array(1))

    list_game match{
      case Game("A", "X") => result += 3 //Need to lose with scissors (rock / scissors)
      case Game("A", "Y") => result += 4 //Need to draw with rock (rock / rock)
      case Game("A", "Z") => result += 8 //Need to win with paper (rock / paper)
      case Game("B", "X") => result += 1 //Need to lose with rock (paper / rock)
      case Game("B", "Y") => result += 5 //Need to draw with paper (paper / paper)
      case Game("B", "Z") => result += 9 //Need to win with scissors (paper / scissors)
      case Game("C", "X") => result += 2 //Need to lose with paper (scissors / paper)
      case Game("C", "Y") => result += 6 //Need to draw with scissors (scissors / scissors)
      case Game("C", "Z") => result += 7 //Need to win with rock (scissors / rock)
    }
  }
  return result
}

println(rock_paper_scissors_strategy("input.txt"))
println(rock_paper_scissors_strategy2("input.txt"))
