import scala.io.Source
import scala.collection.mutable.ArrayBuffer

def define_rocks(): ArrayBuffer[ArrayBuffer[String]] = {
  val rocks = ArrayBuffer[ArrayBuffer[String]]()
  var rock = ArrayBuffer[String]()
  var row:String = ""
  row = "####"
  rock += row
  rocks += rock

  rock = ArrayBuffer[String]()
  row = ".#."
  rock += row
  row = "###"
  rock += row
  row = ".#."
  rock += row
  rocks += rock

  rock = ArrayBuffer[String]()
  row = "..#"
  rock += row
  row = "..#"
  rock += row
  row = "###"
  rock += row
  rocks += rock

  rock = ArrayBuffer[String]()
  row = "#"
  rock += row
  row = "#"
  rock += row
  row = "#"
  rock += row
  row = "#"
  rock += row
  rocks += rock

  rock = ArrayBuffer[String]()
  row = "##"
  rock += row
  row = "##"
  rock += row
  rocks += rock

  rocks
}

def parse_file(filename:String):String = Source.fromResource(filename).getLines().filter(_.nonEmpty).toSeq.head

def create_chamber(): ArrayBuffer[String] = {
  val chamber = ArrayBuffer[String]()
  chamber += "+-------+"
  chamber
}

def create_row(): String = "|.......|"

def prepend_new_rock(chamber:ArrayBuffer[String], rock:ArrayBuffer[String]): ArrayBuffer[String] = {
  val new_chamber = chamber

  for (i <- Range(0, 3)) {
    new_chamber.prepend(create_row())
  }

  for (i <- rock.length -1 to 0 by  -1) {
    var new_row = "|.."

    for (j <- Range(0, rock(i).length)) {
      new_row += rock(i)(j)
    }

    for (_ <- Range(new_row.length, 8)) {
      new_row += "."
    }
    new_row += "|"
    new_chamber.prepend(new_row)
  }

  new_chamber
}

def jet_direction (jets:String, number:Int): String = {
  if (jets(number) == ">"(0)) {
    "right"
  } else {
    "left"
  }
}

def position_rock(rock:ArrayBuffer[String], direction: String, start_position: Int): (Int, Int) = {
  var left_position = start_position
  var right_position = start_position + rock(0).length -1

  if (direction == "left") {
    if ((left_position + 1) >= 1) {
      left_position = left_position - 1
      right_position = right_position - 1
    }
  } else if (direction == "right") {
    if ((right_position + 1) <= 7) {
      left_position = left_position + 1
      right_position = right_position + 1
    }
  }

  (left_position, right_position)
}

def move_in_x(chamber:ArrayBuffer[String], rock:ArrayBuffer[String], direction: String, position: (Int, Int),
           bottom_rock: Int): (ArrayBuffer[String], (Int, Int)) = {

  val new_chamber = chamber
  var new_position = position

  var move_x = true

  for (i <- Range(0, rock.length)) {
    if (direction == "right") {
      for (j <- Range(0, rock(i).length)) {
        if (rock(rock.length -1 -i)(rock(i).length -1 - j) == "#"(0)) {
          if (new_chamber(bottom_rock - i)(new_position._2 + 1) != "."(0)) {
            move_x = false
          }
        }
      }
    } else if (direction == "left") {
      for (j <- Range(0, rock(i).length)) {
        if (rock(rock.length -1 -i)(j) == "#"(0)) {
          if (new_chamber(bottom_rock - i)(new_position._1 - 1) != "."(0)) {
            move_x = false
          }
        }
      }
    }
  }

  if (move_x) {
    new_position = position_rock(rock, direction, position._1)
    var new_row = ""

    if (direction == "right") {
      for (i <- Range(0, rock.length)) {
        new_row = chamber(bottom_rock - i).substring(0, position._1) + "." + rock((rock.length -1) - i) + chamber(bottom_rock - i).substring(new_position._2 + 1, 9)
        new_chamber(bottom_rock - i) = new_row

      }
    } else if (direction == "left") {
      for (i <- Range(0, rock.length)) {
        new_row = chamber(bottom_rock - i).substring(0, new_position._1) + rock(rock.length - 1 - i) + "." + chamber(bottom_rock - i).substring(position._2 + 1, 9)
        new_chamber(bottom_rock - i) = new_row

      }
    }
  }

  (new_chamber, new_position)

}


def move_in_y(chamber:ArrayBuffer[String], rock:ArrayBuffer[String], direction: String, position: (Int, Int),
              bottom_rock: Int): (ArrayBuffer[String], (Int, Int), Int) = {
  val new_chamber = chamber
  var new_row = ""
  var old_row = ""

  for (i <- Range(0, rock.length)) {
    for (j <- Range(0, rock(rock.length -1 - i).length)){
      if (rock(rock.length -1 - i)(j) == "#"(0)) {
        if (chamber(bottom_rock - i + 1)(position._1 + j) != "."(0)) {
          return (chamber, position, bottom_rock)
        }
      } else {
        if (chamber(bottom_rock - i)(position._1 + j) == "#"(0)) {
          return (chamber, position, bottom_rock)
        }
      }
    }

    var temp_row = ""

    for (j <- Range(0, rock(i).length)){
      if (rock(rock.length -1 -i)(j) == "#"(0)) {
        temp_row += "#"
      } else {
        if (chamber(bottom_rock - i + 1)(position._1 + j) == "#"(0)) {
          temp_row += "#"
        } else {
          temp_row += "."
        }
      }
    }

    new_row = chamber(bottom_rock - i +1).substring(0, position._1) + temp_row + chamber(bottom_rock - i +1).substring(position._2 + 1, 9)
    old_row = chamber(bottom_rock - i).substring(0, position._1) + ("." * rock(rock.length -1 -i).length) + chamber(bottom_rock - i).substring(position._2 + 1, 9)
    new_chamber(bottom_rock - i +1) = new_row
    new_chamber(bottom_rock - i) = old_row
  }
  (new_chamber, position, bottom_rock + 1)
}


def one_episode(chamber:ArrayBuffer[String], rock:ArrayBuffer[String], direction: String, position: (Int, Int),
                bottom_rock: Int, print_bug: Boolean): (ArrayBuffer[String], (Int,Int), Int) = {

  if (print_bug) {
    for (i <- chamber){
      println(i)
    }
    println("")
    println(direction)
    println("")
  }
  val move_x = move_in_x(chamber, rock, direction, position, bottom_rock)

  if (print_bug) {
    for (i <- chamber) {
      println(i)
    }
    println("")
  }
  val move_y = move_in_y(move_x._1, rock, direction, move_x._2, bottom_rock)

  if (print_bug) {
    for (i <- chamber) {
      println(i)
    }
    println("")
  }

  (move_y._1, move_y._2, move_y._3)

}


def tetris(filename: String, total_number: Int): Int  = {

  val jets = parse_file(filename)
  val rocks = define_rocks()
  var chamber = create_chamber()

  var rock_number = 0
  var rock = rocks(rock_number)
  chamber = prepend_new_rock(chamber, rock)
  var number_of_rocks = 1


  var bottom_rock = -1 + rock.length
  var start_position = 3
  var number_jet = 0
  var height = 0

  var print_bug = false

  while (number_of_rocks < 1586){

    if (number_of_rocks == 3000) print_bug = true

    val episode = one_episode(chamber, rock, jet_direction(jets, number_jet),
      (start_position, start_position + rock(0).length -1), bottom_rock, print_bug)

    print_bug = false

    chamber = episode._1
    start_position = episode._2._1

    if (episode._3 > bottom_rock) {
      bottom_rock = episode._3
    } else {

      var new_chamber = create_chamber()

      for (i <- chamber.length -1 to 0 by -1) {
        if (chamber(i).contains("#")) {
          new_chamber.prepend(chamber(i))
        }
      }

      height = new_chamber.length -1

      rock_number += 1
      if (rock_number == rocks.length) {
        rock_number = 0
      }

      rock = rocks(rock_number)
      bottom_rock = rock.length -1
      start_position = 3
      chamber = prepend_new_rock(new_chamber, rock)
      number_of_rocks += 1

    }

    number_jet += 1
    if (number_jet == jets.length) {
      number_jet = 0
    }
  }

  println(number_of_rocks -1)
  height
}

// First  star
println(f"Part1: ${tetris(filename = "day17/test.txt", total_number = 2023)}")
//println(f"Part2: ${sand2("day15/test.txt", y_coord = 10)}")
