import scala.io.Source
import scala.collection.mutable

class TestCube(){
  def position_in_cube(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): String = {

    if (((position._1 >= 0) && (position._1 < cube_boundaries._1._2 / 3)) &&
      ((position._2 >= cube_boundaries._2._2 / 2) && (position._2 < (cube_boundaries._2._2 / 4) * 3))) {
      return "A"
    } else if (((position._1 >= cube_boundaries._1._2 / 3) && (position._1 < (cube_boundaries._1._2 / 3) * 2)) &&
      ((position._2 >= 0) && (position._2 < cube_boundaries._2._2 / 4))) {
      return "B"
    } else if (((position._1 >= cube_boundaries._1._2 / 3) && (position._1 < (cube_boundaries._1._2 / 3) * 2)) &&
      ((position._2 >= cube_boundaries._2._2 / 4) && (position._2 < cube_boundaries._2._2 / 2))) {
      return "C"
    } else if (((position._1 >= cube_boundaries._1._2 / 3) && (position._1 < (cube_boundaries._1._2 / 3) * 2)) &&
      ((position._2 >= cube_boundaries._2._2 / 2) && (position._2 < (cube_boundaries._2._2 / 4) * 3))) {
      return "D"
    } else if (((position._1 >= (cube_boundaries._1._2 / 3) * 2) && (position._1 < cube_boundaries._1._2)) &&
      ((position._2 >= cube_boundaries._2._2 / 2) && (position._2 < (cube_boundaries._2._2 / 4) * 3))) {
      return "E"
    } else {
      return "F"
    }
  }

  def fromA_toB(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): ((Int, Int), String) = {
    val new_x = ((cube_boundaries._2._2 / 4) - 1) - (position._2 - (cube_boundaries._2._2 / 2))
    val new_y = cube_boundaries._1._2 / 3
    return ((new_x, new_y), "D")
  }

  def fromA_toC(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): ((Int, Int), String) = {
    val new_x = position._1 + (cube_boundaries._2._2 / 4)
    val new_y = cube_boundaries._1._2 / 3
    return ((new_x, new_y), "D")
  }

  def fromA_toF(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): ((Int, Int), String) = {
    val new_x = cube_boundaries._2._2 - 1
    val new_y = cube_boundaries._1._2 - position._1 - 1
    return ((new_x, new_y), "L")
  }

  def fromB_toA(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): ((Int, Int), String) = {
    val new_x = ((cube_boundaries._2._2 / 4) * 3) - 1 - position._2
    val new_y = 0
    return ((new_x, new_y), "D")
  }

  def fromB_toF(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): ((Int, Int), String) = {
    val new_x = (cube_boundaries._2._2 - (position._1 - cube_boundaries._1._2 / 3)) - 1
    val new_y = cube_boundaries._1._2 - 1
    return ((new_x, new_y), "U")
  }

  def fromB_toE(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): ((Int, Int), String) = {
    val new_x = (cube_boundaries._2._2 / 4 * 3) - position._2 - 1
    val new_y = cube_boundaries._1._2 - 1
    return ((new_x, new_y), "U")
  }

  def fromC_toA(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): ((Int, Int), String) = {
    val new_x = cube_boundaries._2._2 / 2
    val new_y = position._2 - (cube_boundaries._2._2 / 4)
    return ((new_x, new_y), "R")
  }

  def fromC_toE(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): ((Int, Int), String) = {
    val new_x = cube_boundaries._2._2 / 2
    val new_y = cube_boundaries._1._2 - (position._2 - (cube_boundaries._2._2 / 4)) - 1
    return ((new_x, new_y), "R")
  }

  def fromD_toF(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): ((Int, Int), String) = {
    val new_x = cube_boundaries._2._2 - (position._1 - cube_boundaries._1._2 / 3) - 1
    val new_y = (cube_boundaries._1._2 / 3) * 2
    return ((new_x, new_y), "D")
  }

  def fromE_toB(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): ((Int, Int), String) = {
    val new_x = ((cube_boundaries._2._2 / 4) * 3) - 1 - position._2
    val new_y = ((cube_boundaries._1._2 / 3) * 2) - 1
    return ((new_x, new_y), "U")
  }

  def fromE_toC(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): ((Int, Int), String) = {
    val new_x = ((cube_boundaries._2._2 / 2) - 1) - (position._1 - (cube_boundaries._1._2 / 3) * 2)
    val new_y = ((cube_boundaries._1._2 / 3) * 2) - 1
    return ((new_x, new_y), "U")
  }

  def fromF_toA(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): ((Int, Int), String) = {
    val new_x = (cube_boundaries._2._2 / 4) * 3
    val new_y = cube_boundaries._1._2 - 1 - position._1
    return ((new_x, new_y), "L")
  }

  def fromF_toB(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): ((Int, Int), String) = {
    val new_x = 0
    val new_y = (((cube_boundaries._1._2 / 3) * 2) - 1) - (position._1 - ((cube_boundaries._2._2 / 4) * 3))
    return ((new_x, new_y), "D")
  }

  def fromF_toD(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): ((Int, Int), String) = {
    val new_x = ((cube_boundaries._2._2 / 4) * 3) - 1
    val new_y = (((cube_boundaries._1._2 / 3) * 2) - 1) - (position._1 - ((cube_boundaries._2._2 / 4) * 3))
    return ((new_x, new_y), "L")
  }

  def change_heading(heading: String, turn: String): String = {
    var new_heading: String = ""

    if (turn != "F") {
      if (heading == "U") {
        if (turn == "L") {
          new_heading = "L"
        } else {
          new_heading = "R"
        }
      } else if (heading == "D") {
        if (turn == "L") {
          new_heading = "R"
        } else {
          new_heading = "L"
        }
      } else if (heading == "R") {
        if (turn == "L") {
          new_heading = "U"
        } else {
          new_heading = "D"
        }
      } else if (heading == "L") {
        if (turn == "L") {
          new_heading = "D"
        } else {
          new_heading = "U"
        }
      }
    } else {
      new_heading = heading
    }

    new_heading
  }

  def solve_part_2(map: mutable.ListBuffer[((Int, Int), mutable.ListBuffer[Int])],
                        instructions: mutable.ListBuffer[(Int, String)], position: (Int, Int),
                        boundaries: ((Int, Int), (Int, Int))): (String, (Int, Int)) = {

    var facing: String = "R"
    var x_pos: Int = position._1
    var y_pos: Int = position._2

    var cube_face = this.position_in_cube((y_pos, x_pos), boundaries)

    for (instruction <- instructions) {

      for (_ <- Range(0, instruction._1)) {
        facing match {
          case "U" =>
            if (((y_pos - 1) < 0) || ((x_pos < map(y_pos - 1)._1._1) || (x_pos > map(y_pos - 1)._1._2))) {

              cube_face = this.position_in_cube((y_pos, x_pos), boundaries)

              cube_face match {
                case "A" => {
                  val new_position = this.fromA_toB((y_pos, x_pos), boundaries)

                  if (!map(new_position._1._2)._2.contains(new_position._1._1)) {
                    y_pos = new_position._1._2
                    x_pos = new_position._1._1
                    facing = new_position._2
                  }
                }
                case "B" => {
                  var new_position = this.fromB_toA((y_pos, x_pos), boundaries)
                  if (!map(new_position._1._2)._2.contains(new_position._1._1)) {
                    y_pos = new_position._1._2
                    x_pos = new_position._1._1
                    facing = new_position._2
                  }
                }
                case "C" => {
                  var new_position = this.fromC_toA((y_pos, x_pos), boundaries)
                  if (!map(new_position._1._2)._2.contains(new_position._1._1)) {
                    y_pos = new_position._1._2
                    x_pos = new_position._1._1
                    facing = new_position._2
                  }
                }
                case "F" => {
                  var new_position = this.fromF_toD((y_pos, x_pos), boundaries)
                  if (!map(new_position._1._2)._2.contains(new_position._1._1)) {
                    y_pos = new_position._1._2
                    x_pos = new_position._1._1
                    facing = new_position._2
                  }
                }
              }
            } else {
              if (map(y_pos - 1)._2.contains(x_pos)) {
                y_pos = y_pos
              } else {
                y_pos -= 1
              }
            }

          case "D" =>
            if (((y_pos + 1) >= map.length) || ((x_pos < map(y_pos + 1)._1._1) || (x_pos > map(y_pos + 1)._1._2))) {
              cube_face = this.position_in_cube((y_pos, x_pos), boundaries)

              cube_face match {
                case "B" => {
                  val new_position = fromB_toE((y_pos, x_pos), boundaries)

                  if (!map(new_position._1._2)._2.contains(new_position._1._1)) {
                    y_pos = new_position._1._2
                    x_pos = new_position._1._1
                    facing = new_position._2
                  }
                }
                case "C" => {
                  var new_position = fromC_toE((y_pos, x_pos), boundaries)
                  if (!map(new_position._1._2)._2.contains(new_position._1._1)) {
                    y_pos = new_position._1._2
                    x_pos = new_position._1._1
                    facing = new_position._2
                  }
                }
                case "E" => {
                  var new_position = fromE_toB((y_pos, x_pos), boundaries)
                  if (!map(new_position._1._2)._2.contains(new_position._1._1)) {
                    y_pos = new_position._1._2
                    x_pos = new_position._1._1
                    facing = new_position._2
                  }
                }
                case "F" => {
                  var new_position = fromF_toB((y_pos, x_pos), boundaries)
                  if (!map(new_position._1._2)._2.contains(new_position._1._1)) {
                    y_pos = new_position._1._2
                    x_pos = new_position._1._1
                    facing = new_position._2
                  }
                }
              }
            } else {
              if (map(y_pos + 1)._2.contains(x_pos)) {
                y_pos = y_pos
              } else {
                y_pos += 1
              }
            }

          case "L" =>
            if ((x_pos - 1) < map(y_pos)._1._1) {
              cube_face = position_in_cube((y_pos, x_pos), boundaries)

              cube_face match {
                case "A" => {
                  val new_position = fromA_toC((y_pos, x_pos), boundaries)

                  if (!map(new_position._1._2)._2.contains(new_position._1._1)) {
                    y_pos = new_position._1._2
                    x_pos = new_position._1._1
                    facing = new_position._2
                  }
                }
                case "B" => {
                  var new_position = fromB_toF((y_pos, x_pos), boundaries)
                  if (!map(new_position._1._2)._2.contains(new_position._1._1)) {
                    y_pos = new_position._1._2
                    x_pos = new_position._1._1
                    facing = new_position._2
                  }
                }
                case "E" => {
                  var new_position = fromE_toC((y_pos, x_pos), boundaries)
                  if (!map(new_position._1._2)._2.contains(new_position._1._1)) {
                    y_pos = new_position._1._2
                    x_pos = new_position._1._1
                    facing = new_position._2
                  }
                }
              }
            } else {
              if (map(y_pos)._2.contains(x_pos - 1)) {
                x_pos = x_pos
              } else {
                x_pos -= 1
              }
            }

          case "R" =>
            if ((x_pos + 1) > map(y_pos)._1._2) {
              cube_face = position_in_cube((y_pos, x_pos), boundaries)
              cube_face match {
                case "A" => {
                  val new_position = fromA_toF((y_pos, x_pos), boundaries)

                  if (!map(new_position._1._2)._2.contains(new_position._1._1)) {
                    y_pos = new_position._1._2
                    x_pos = new_position._1._1
                    facing = new_position._2
                  }
                }
                case "D" => {
                  var new_position = fromD_toF((y_pos, x_pos), boundaries)
                  if (!map(new_position._1._2)._2.contains(new_position._1._1)) {
                    y_pos = new_position._1._2
                    x_pos = new_position._1._1
                    facing = new_position._2
                  }
                }
                case "F" => {
                  var new_position = fromF_toA((y_pos, x_pos), boundaries)
                  if (!map(new_position._1._2)._2.contains(new_position._1._1)) {
                    y_pos = new_position._1._2
                    x_pos = new_position._1._1
                    facing = new_position._2
                  }
                }
              }
            } else {
              if (map(y_pos)._2.contains(x_pos + 1)) {
                x_pos = x_pos
              } else {
                x_pos += 1
              }
            }
        }

      }
      facing = this.change_heading(facing, instruction._2)
    }

    (facing, (x_pos, y_pos))

  }
}

class InputCube(){
  def position_in_cube(position: (Int, Int), cube_boundaries: ((Int, Int), (Int, Int))): String = {

    if (((position._1 >= 0) && (position._1 < cube_boundaries._1._2 / 3)) &&
      ((position._2 >= cube_boundaries._2._2 / 2) && (position._2 < (cube_boundaries._2._2 / 4) * 3))) {
      // Done
      return "A"
    } else if (((position._1 >= cube_boundaries._1._2 / 3) && (position._1 < (cube_boundaries._1._2 / 3) * 2)) &&
      ((position._2 >= 0) && (position._2 < cube_boundaries._2._2 / 4))) {

      return "B"
    } else if (((position._1 >= cube_boundaries._1._2 / 3) && (position._1 < (cube_boundaries._1._2 / 3) * 2)) &&
      ((position._2 >= cube_boundaries._2._2 / 4) && (position._2 < cube_boundaries._2._2 / 2))) {
      return "C"
    } else if (((position._1 >= cube_boundaries._1._2 / 3) && (position._1 < (cube_boundaries._1._2 / 3) * 2)) &&
      ((position._2 >= cube_boundaries._2._2 / 2) && (position._2 < (cube_boundaries._2._2 / 4) * 3))) {
      return "D"
    } else if (((position._1 >= (cube_boundaries._1._2 / 3) * 2) && (position._1 < cube_boundaries._1._2)) &&
      ((position._2 >= cube_boundaries._2._2 / 2) && (position._2 < (cube_boundaries._2._2 / 4) * 3))) {
      return "E"
    } else {
      return "F"
    }
  }
}

def parse_file(filename:String):(mutable.ListBuffer[((Int, Int), mutable.ListBuffer[Int])], mutable.ListBuffer[(Int, String)], Int) = {

  var parse_map: Boolean = true
  val map: mutable.ListBuffer[((Int, Int), mutable.ListBuffer[Int])] = mutable.ListBuffer()
  val instructions: mutable.ListBuffer[(Int, String)] = mutable.ListBuffer()
  var max_x: Int = 0

  for (line <- Source.fromResource(filename).getLines().filter(_.nonEmpty).toSeq) {
    if (line.contains("#") || line.contains(".")){
      parse_map = true
    } else {
      parse_map = false
    }

    var min_boundaries: Int = 0
    var max_boundaries: Int = 0
    val walls_coord: mutable.ListBuffer[Int] = mutable.ListBuffer()
    var parse_boundaries: Boolean = true
    var boundaries: (Int, Int) = (0, 0)

    if (parse_map){
      for ((map_position, i) <- line.zipWithIndex){
        if (((map_position == "#"(0)) || (map_position == "."(0))) && parse_boundaries){
          min_boundaries = i
          max_boundaries = line.length - 1
          parse_boundaries = false
          if (max_boundaries > max_x) max_x = max_boundaries
          boundaries = (min_boundaries, max_boundaries)
        }

        if (map_position == "#"(0)){
          walls_coord += i
        }
      }

      map += ((boundaries, walls_coord))

    } else {

      var digit: String = ""
      var instruction: (Int, String) = (0, "")

      for (character <- line){
        if (character.isDigit){
          digit += character
        } else {
          var characters: mutable.ListBuffer[Char] = mutable.ListBuffer('R', 'L', 'U', 'D')
          if (!characters.contains(character)) {
            println(character)
          }
          instruction = (digit.toInt, character.toString)
          digit = ""
          instructions += instruction
        }
      }

      instruction = (digit.toInt, "F")
      instructions += instruction
    }
  }

  (map, instructions, max_x +1)
}

def solve_part_1(map: mutable.ListBuffer[((Int, Int), mutable.ListBuffer[Int])],
                 instructions:mutable.ListBuffer[(Int, String)], position: (Int, Int) ): (String, (Int, Int)) = {

  var facing: String = "R"
  var x_pos: Int = position._1
  var y_pos: Int = position._2
  var cube = new TestCube

  for (instruction <- instructions) {
    facing match {
      case "U" =>
        for (_ <- Range(0, instruction._1)) {
          if (((y_pos - 1) < 0) || ((x_pos < map(y_pos - 1)._1._1) || (x_pos > map(y_pos - 1)._1._2))) {
            val y_old = y_pos.intValue
            y_pos = map.length - 1

            while ((x_pos < map(y_pos)._1._1) || (x_pos > map(y_pos)._1._2)) {
              y_pos -= 1

              if (map(y_pos)._2.contains(x_pos)) {
                y_pos = y_old
              }
            }
          } else {
            if (map(y_pos - 1)._2.contains(x_pos)) {
              y_pos = y_pos
            } else {
              y_pos -= 1
            }
          }
        }
      case "D" =>
        for (_ <- Range(0, instruction._1)) {
          if (((y_pos + 1) >= map.length) || ((x_pos < map(y_pos + 1)._1._1) || (x_pos > map(y_pos + 1)._1._2))) {
            val y_old = y_pos.intValue
            y_pos = 0

            while ((x_pos < map(y_pos)._1._1) || (x_pos > map(y_pos)._1._2)) {
              y_pos += 1

              if (map(y_pos)._2.contains(x_pos)) {
                y_pos = y_old
              }
            }
          } else {
            if (map(y_pos + 1)._2.contains(x_pos)) {
              y_pos = y_pos
            } else {
              y_pos += 1
            }
          }
        }
      case "L" =>
        for (_ <- Range(0, instruction._1)) {
          if (map(y_pos)._2.contains(x_pos - 1)) {
            x_pos = x_pos
          } else {
            if ((x_pos - 1) < map(y_pos)._1._1) {
              if (map(y_pos)._2.contains(map(y_pos)._1._2)) {
                x_pos = x_pos
              } else {
                x_pos = map(y_pos)._1._2
              }
            } else {
              x_pos -= 1
            }
          }
        }
      case "R" =>
        for (_ <- Range(0, instruction._1)) {
          if (map(y_pos)._2.contains(x_pos + 1)) {
            x_pos = x_pos
          } else {
            if ((x_pos + 1) > map(y_pos)._1._2) {
              if (map(y_pos)._2.contains(map(y_pos)._1._1)) {
                x_pos = x_pos
              } else {
                x_pos = map(y_pos)._1._1
              }
            } else {
              x_pos += 1
            }
          }
        }
    }

    facing = cube.change_heading(facing, instruction._2)
  }

  (facing, (x_pos, y_pos))
}

def find_password(filename: String, part:Int): Long  = {

  val parsing = parse_file(filename)
  val map = parsing._1
  val instructions = parsing._2

  var facing: String = "R"
  var y_pos = 0
  var x_pos = map.head._1._1
  var cube_face: String = ""
  val boundaries = ((0, map.length), (0, parsing._3))

  if (part == 1){

    val result = solve_part_1(map, instructions, (x_pos, y_pos))
    facing = result._1
    x_pos = result._2._1
    y_pos = result._2._2

    var facing_value: Int = 0
    facing match {
      case "U" => facing_value = 3
      case "D" => facing_value = 1
      case "L" => facing_value = 2
      case "R" => facing_value = 0
    }

    return (1000 * (y_pos + 1)) + (4 * (x_pos + 1)) + (facing_value)
  } else {

    if (filename.contains("test")){
      var cube = new TestCube()
      cube_face = cube.position_in_cube((y_pos, x_pos), boundaries)

      val result = cube.solve_part_2(map, instructions, (x_pos, y_pos), boundaries)
      facing = result._1
      x_pos = result._2._1
      y_pos = result._2._2

    } else {
      var cube = new InputCube()
    }

    var facing_value: Int = 0
    facing match {
      case "U" => facing_value = 3
      case "D" => facing_value = 1
      case "L" => facing_value = 2
      case "R" => facing_value = 0
    }

    return (1000 * (y_pos + 1)) + (4 * (x_pos + 1)) + (facing_value)

  }
}

// First and second star
println(f"Part1 (Test): password = ${find_password(filename = "day22/test.txt", part = 1)}")
println(f"Part1: password = ${find_password(filename = "day22/input.txt", part = 1)}")
println(f"Part2 (Test): password = ${find_password(filename = "day22/test.txt", part = 2)}")
//println(f"Part2: password = ${find_password(filename = "day22/input.txt", part = 2)}")
