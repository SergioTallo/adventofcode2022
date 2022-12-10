import scala.io.Source
import scala.collection.mutable.ArrayBuffer


// Use same function for both stars, the only thing that changes is the bodysize
def pixels(filename:String):Int= {
  var cycle:Int = 1
  var register_x:Int = 1
  var check:Int = 20
  var signal_strength:Int = 0
  var sprite_position:Int = 1
  var crt_position:Int = 0
  var crt_line:String = ""

  println("")
  for (line <- Source.fromFile(filename).getLines){
    if (line contains "noop"){

      // This is for the second star
      sprite_position = register_x

      if (crt_position <= (register_x + 1) && crt_position >= (register_x - 1)) {
        crt_line += "#"
      } else {
        crt_line += "."
      }
      if (crt_position == 39) {
        println(crt_line)
        crt_line = ""
        crt_position = 0
      } else {
        crt_position += 1
      }

      cycle += 1

      // This is for the first star
      if (cycle == check) {
        signal_strength += (register_x * cycle)
        check += 40
      }

    } else {

      // This is for the second star
      sprite_position = register_x

      if (crt_position <= (register_x + 1) && crt_position >= (register_x - 1)) {
        crt_line += "#"
      } else {
        crt_line += "."
      }
      if (crt_position == 39) {
        println(crt_line)
        crt_line = ""
        crt_position = 0
      } else {
        crt_position += 1
      }

      cycle += 1

      // This is for the first star
      if (cycle == check) {
        signal_strength += (register_x * cycle)
        check += 40
      }

      // This is for the second star
      if (crt_position <= (register_x + 1) && crt_position >= (register_x - 1)) {
        crt_line += "#"
      } else {
        crt_line += "."
      }
      if (crt_position == 39) {
        println(crt_line)
        crt_line = ""
        crt_position = 0
      } else {
        crt_position += 1
      }

      cycle += 1

      // Update the register (for both stars)
      register_x += line.split(" ")(1).toInt

      // This is for the first star
      if (cycle == check) {
        signal_strength += (register_x * cycle)
        check += 40
      }
    }
  }

  println("")
  signal_strength

}

// First and second star
println(f"signal strength: ${pixels("day10/input.txt")}")

