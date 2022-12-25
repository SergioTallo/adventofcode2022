import scala.io.Source
import scala.collection.mutable

def parse_file(filename:String):(mutable.Map[Int, (Int, Int)], mutable.ListBuffer[(Int, Int)],
  ((Int, Int), (Int, Int))) = {

  val map: mutable.Map[Int, (Int, Int)] = mutable.Map()
  val positions: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer()
  var count: Int = 0
  var max_x = 0
  var max_y = 0

  for ((line, i) <- Source.fromResource(filename).getLines().filter(_.nonEmpty).toSeq.zipWithIndex) {
    for ((char, j) <- line.zipWithIndex) {
      if (char == '#') {
        map += count -> (i, j)
        val coordinates = (i, j)
        positions += coordinates
        count += 1
      }
      if (j > max_x) max_x = j
    }
    if (i > max_y) max_y = i
  }

  (map, positions, ((0, max_y), (0, max_x)))

}

def check_neighbors(coordinate: (Int, Int), positions: mutable.ListBuffer[(Int, Int)]): Boolean = {

  if (positions.contains((coordinate._1 - 1, coordinate._2 -1)) || positions.contains((coordinate._1 - 1, coordinate._2)) ||
    positions.contains((coordinate._1 -1, coordinate._2 + 1)) || positions.contains((coordinate._1, coordinate._2 -1)) ||
    positions.contains((coordinate._1, coordinate._2 + 1)) || positions.contains((coordinate._1 +1, coordinate._2 - 1)) ||
    positions.contains((coordinate._1 +1, coordinate._2)) || positions.contains((coordinate._1 +1, coordinate._2 +1))) {
    true
  } else {
    false
  }
}

def north_first(map: mutable.Map[Int, (Int, Int)], positions: mutable.ListBuffer[(Int, Int)],
                boundaries: ((Int, Int), (Int, Int))): (mutable.ListBuffer[(Int, Int)], ((Int, Int), (Int, Int))) ={

  val new_positions: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer()
  var new_boundaries: ((Int, Int), (Int, Int)) = boundaries

  var max_x = -10000
  var max_y = -10000
  var min_x = 10000
  var min_y = 10000

  for (i <- positions) {
    val coordinates = i

    if (check_neighbors(i, positions)){
        // North
      if (!positions.contains((coordinates._1 - 1, coordinates._2 - 1)) &&
        !positions.contains((coordinates._1 - 1, coordinates._2)) &&
        !positions.contains((coordinates._1 - 1, coordinates._2 + 1))) {

        new_positions += ((coordinates._1 - 1, coordinates._2))
        if (coordinates._1 - 1 < new_boundaries._1._1) new_boundaries = ((coordinates._1 - 1, new_boundaries._1._2), new_boundaries._2)
        if (coordinates._1 - 1 < min_y) min_y = coordinates._1 - 1
        if (coordinates._1 - 1 > max_y) max_y = coordinates._1 - 1
        if (coordinates._2 < min_x) min_x = coordinates._2
        if (coordinates._2 > max_x) max_x = coordinates._2

        // South
      } else if (!positions.contains((coordinates._1 + 1, coordinates._2 - 1)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2 + 1))) {

        new_positions += ((coordinates._1 + 1, coordinates._2))
        if (coordinates._1 + 1 > new_boundaries._1._2) new_boundaries = ((new_boundaries._1._1, coordinates._1 + 1), new_boundaries._2)
        if (coordinates._1 + 1 < min_y) min_y = coordinates._1 + 1
        if (coordinates._1 + 1 > max_y) max_y = coordinates._1 + 1
        if (coordinates._2 < min_x) min_x = coordinates._2
        if (coordinates._2 > max_x) max_x = coordinates._2

        // West
      } else if (!positions.contains((coordinates._1 - 1, coordinates._2 -1)) &&
        !positions.contains((coordinates._1, coordinates._2 -1)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2 -1))) {

        new_positions += ((coordinates._1, coordinates._2 -1))
        if (coordinates._2 -1 < new_boundaries._2._1) new_boundaries = (new_boundaries._1, (coordinates._2 -1, new_boundaries._2._2))
        if (coordinates._1 < min_y) min_y = coordinates._1
        if (coordinates._1 > max_y) max_y = coordinates._1
        if (coordinates._2 -1 < min_x) min_x = coordinates._2 -1
        if (coordinates._2 -1 > max_x) max_x = coordinates._2 -1

        // East
      } else if (!positions.contains((coordinates._1 - 1, coordinates._2 + 1)) &&
        !positions.contains((coordinates._1, coordinates._2 + 1)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2 + 1))) {

        new_positions += ((coordinates._1, coordinates._2 + 1))
        if (coordinates._2 + 1 > new_boundaries._2._2) new_boundaries = (new_boundaries._1, (new_boundaries._2._1, coordinates._2 +1))
        if (coordinates._1 < min_y) min_y = coordinates._1
        if (coordinates._1 > max_y) max_y = coordinates._1
        if (coordinates._2 +1 < min_x) min_x = coordinates._2 +1
        if (coordinates._2 +1 > max_x) max_x = coordinates._2 +1

      } else {
        if (coordinates._1 < min_y) min_y = coordinates._1
        if (coordinates._1 > max_y) max_y = coordinates._1
        if (coordinates._2 < min_x) min_x = coordinates._2
        if (coordinates._2 > max_x) max_x = coordinates._2
        new_positions += coordinates
      }
    } else {
      if (coordinates._1 < min_y) min_y = coordinates._1
      if (coordinates._1 > max_y) max_y = coordinates._1
      if (coordinates._2 < min_x) min_x = coordinates._2
      if (coordinates._2 > max_x) max_x = coordinates._2
      new_positions += coordinates
    }
  }

  (new_positions, ((min_y, max_y), (min_x, max_x)))

}

def south_first(map: mutable.Map[Int, (Int, Int)], positions: mutable.ListBuffer[(Int, Int)],
                boundaries: ((Int, Int), (Int, Int))): (mutable.ListBuffer[(Int, Int)], ((Int, Int), (Int, Int))) = {

  val new_positions: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer()
  var new_boundaries: ((Int, Int), (Int, Int)) = boundaries
  var max_x = -1000
  var max_y = -1000
  var min_x = 10000
  var min_y = 10000

  for (i <- positions) {
    val coordinates = i

    if (check_neighbors(i, positions)) {
      // South
      if (!positions.contains((coordinates._1 + 1, coordinates._2 - 1)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2 + 1))) {

        new_positions += ((coordinates._1 + 1, coordinates._2))
        if (coordinates._1 + 1 > new_boundaries._1._2) new_boundaries = ((new_boundaries._1._1, coordinates._1 + 1), new_boundaries._2)
        if (coordinates._1 + 1 < min_y) min_y = coordinates._1 + 1
        if (coordinates._1 + 1 > max_y) max_y = coordinates._1 + 1
        if (coordinates._2 < min_x) min_x = coordinates._2
        if (coordinates._2 > max_x) max_x = coordinates._2

        // West
      } else if (!positions.contains((coordinates._1 - 1, coordinates._2 - 1)) &&
        !positions.contains((coordinates._1, coordinates._2 - 1)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2 - 1))) {

        new_positions += ((coordinates._1, coordinates._2 - 1))
        if (coordinates._2 - 1 < new_boundaries._2._1) new_boundaries = (new_boundaries._1, (coordinates._2 - 1, new_boundaries._2._2))
        if (coordinates._1 < min_y) min_y = coordinates._1
        if (coordinates._1 > max_y) max_y = coordinates._1
        if (coordinates._2 -1 < min_x) min_x = coordinates._2 -1
        if (coordinates._2 -1 > max_x) max_x = coordinates._2 -1

        // East
      } else if (!positions.contains((coordinates._1 - 1, coordinates._2 + 1)) &&
        !positions.contains((coordinates._1, coordinates._2 + 1)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2 + 1))) {

        new_positions += ((coordinates._1, coordinates._2 + 1))
        if (coordinates._2 + 1 > new_boundaries._2._2) new_boundaries = (new_boundaries._1, (new_boundaries._2._1, coordinates._2 +1))
        if (coordinates._1 < min_y) min_y = coordinates._1
        if (coordinates._1 > max_y) max_y = coordinates._1
        if (coordinates._2 + 1 < min_x) min_x = coordinates._2 + 1
        if (coordinates._2 + 1 > max_x) max_x = coordinates._2 + 1

      } else if (!positions.contains((coordinates._1 - 1, coordinates._2 - 1)) &&
        !positions.contains((coordinates._1 - 1, coordinates._2)) &&
        !positions.contains((coordinates._1 - 1, coordinates._2 + 1))) {

        new_positions += ((coordinates._1 - 1, coordinates._2))
        if (coordinates._1 - 1 < new_boundaries._1._1) new_boundaries = ((coordinates._1 - 1, new_boundaries._1._2), new_boundaries._2)
        if (coordinates._1 - 1 < min_y) min_y = coordinates._1 - 1
        if (coordinates._1 - 1 > max_y) max_y = coordinates._1 - 1
        if (coordinates._2 < min_x) min_x = coordinates._2
        if (coordinates._2 > max_x) max_x = coordinates._2

      } else {
        if (coordinates._1 < min_y) min_y = coordinates._1
        if (coordinates._1 > max_y) max_y = coordinates._1
        if (coordinates._2 < min_x) min_x = coordinates._2
        if (coordinates._2 > max_x) max_x = coordinates._2
        new_positions += coordinates
      }
    } else {
      if (coordinates._1 < min_y) min_y = coordinates._1
      if (coordinates._1 > max_y) max_y = coordinates._1
      if (coordinates._2 < min_x) min_x = coordinates._2
      if (coordinates._2 > max_x) max_x = coordinates._2
      new_positions += coordinates
    }
  }

  (new_positions, ((min_y, max_y), (min_x, max_x)))

}

def west_first(map: mutable.Map[Int, (Int, Int)], positions: mutable.ListBuffer[(Int, Int)],
                boundaries: ((Int, Int), (Int, Int))): (mutable.ListBuffer[(Int, Int)], ((Int, Int), (Int, Int))) = {

  val new_positions: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer()
  var new_boundaries: ((Int, Int), (Int, Int)) = boundaries
  var max_x = -1000
  var max_y = -1000
  var min_x = 10000
  var min_y = 10000

  for (i <- positions) {
    val coordinates = i

    if (check_neighbors(i, positions)) {
        //West
      if (!positions.contains((coordinates._1 - 1, coordinates._2 - 1)) &&
        !positions.contains((coordinates._1, coordinates._2 - 1)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2 - 1))) {

        new_positions += ((coordinates._1, coordinates._2 - 1))
        if (coordinates._2 - 1 < new_boundaries._2._1) new_boundaries = (new_boundaries._1, (coordinates._2 - 1, new_boundaries._2._2))
        if (coordinates._1 < min_y) min_y = coordinates._1
        if (coordinates._1 > max_y) max_y = coordinates._1
        if (coordinates._2 - 1 < min_x) min_x = coordinates._2 - 1
        if (coordinates._2 - 1 > max_x) max_x = coordinates._2 - 1

      } else if (!positions.contains((coordinates._1 - 1, coordinates._2 + 1)) &&
        !positions.contains((coordinates._1, coordinates._2 + 1)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2 + 1))) {

        new_positions += ((coordinates._1, coordinates._2 + 1))
        if (coordinates._2 + 1 > new_boundaries._2._2) new_boundaries = (new_boundaries._1, (new_boundaries._2._1, coordinates._2 +1))
        if (coordinates._1 < min_y) min_y = coordinates._1
        if (coordinates._1 > max_y) max_y = coordinates._1
        if (coordinates._2 + 1 < min_x) min_x = coordinates._2 + 1
        if (coordinates._2 + 1 > max_x) max_x = coordinates._2 + 1

      } else if (!positions.contains((coordinates._1 - 1, coordinates._2 - 1)) &&
        !positions.contains((coordinates._1 - 1, coordinates._2)) &&
        !positions.contains((coordinates._1 - 1, coordinates._2 + 1))) {

        new_positions += ((coordinates._1 - 1, coordinates._2))
        if (coordinates._1 - 1 < new_boundaries._1._1) new_boundaries = ((coordinates._1 - 1, new_boundaries._1._2), new_boundaries._2)
        if (coordinates._1 - 1 < min_y) min_y = coordinates._1 - 1
        if (coordinates._1 - 1 > max_y) max_y = coordinates._1 - 1
        if (coordinates._2 < min_x) min_x = coordinates._2
        if (coordinates._2 > max_x) max_x = coordinates._2

      } else if (!positions.contains((coordinates._1 + 1, coordinates._2 - 1)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2 + 1))) {

        new_positions += ((coordinates._1 + 1, coordinates._2))
        if (coordinates._1 + 1 > new_boundaries._1._2) new_boundaries = ((new_boundaries._1._1, coordinates._1 + 1), new_boundaries._2)
        if (coordinates._1 + 1 < min_y) min_y = coordinates._1 + 1
        if (coordinates._1 + 1 > max_y) max_y = coordinates._1 + 1
        if (coordinates._2 < min_x) min_x = coordinates._2
        if (coordinates._2 > max_x) max_x = coordinates._2

      } else {
        if (coordinates._1 < min_y) min_y = coordinates._1
        if (coordinates._1 > max_y) max_y = coordinates._1
        if (coordinates._2 < min_x) min_x = coordinates._2
        if (coordinates._2 > max_x) max_x = coordinates._2
        new_positions += coordinates
      }
    } else {
      if (coordinates._1 < min_y) min_y = coordinates._1
      if (coordinates._1 > max_y) max_y = coordinates._1
      if (coordinates._2 < min_x) min_x = coordinates._2
      if (coordinates._2 > max_x) max_x = coordinates._2
      new_positions += coordinates
    }
  }

  (new_positions, ((min_y, max_y), (min_x, max_x)))

}

def east_first(map: mutable.Map[Int, (Int, Int)], positions: mutable.ListBuffer[(Int, Int)],
                boundaries: ((Int, Int), (Int, Int))): (mutable.ListBuffer[(Int, Int)], ((Int, Int), (Int, Int))) = {

  val new_positions: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer()
  var new_boundaries: ((Int, Int), (Int, Int)) = boundaries
  var max_x = -1000
  var max_y = -1000
  var min_x = 10000
  var min_y = 10000

  for (i <- positions) {
    val coordinates = i

    if (check_neighbors(i, positions)) {
      if (!positions.contains((coordinates._1 - 1, coordinates._2 + 1)) &&
        !positions.contains((coordinates._1, coordinates._2 + 1)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2 + 1))) {

        new_positions += ((coordinates._1, coordinates._2 + 1))
        if (coordinates._2 + 1 > new_boundaries._2._2) new_boundaries = (new_boundaries._1, (new_boundaries._2._1, coordinates._2 +1))
        if (coordinates._1 < min_y) min_y = coordinates._1
        if (coordinates._1 > max_y) max_y = coordinates._1
        if (coordinates._2 + 1 < min_x) min_x = coordinates._2 + 1
        if (coordinates._2 + 1 > max_x) max_x = coordinates._2 + 1

      } else if (!positions.contains((coordinates._1 - 1, coordinates._2 - 1)) &&
        !positions.contains((coordinates._1 - 1, coordinates._2)) &&
        !positions.contains((coordinates._1 - 1, coordinates._2 + 1))) {

        new_positions += ((coordinates._1 - 1, coordinates._2))
        if (coordinates._1 - 1 < new_boundaries._1._1) new_boundaries = ((coordinates._1 - 1, new_boundaries._1._2), new_boundaries._2)
        if (coordinates._1 - 1 < min_y) min_y = coordinates._1 - 1
        if (coordinates._1 - 1 > max_y) max_y = coordinates._1 - 1
        if (coordinates._2 < min_x) min_x = coordinates._2
        if (coordinates._2 > max_x) max_x = coordinates._2

      } else if (!positions.contains((coordinates._1 + 1, coordinates._2 - 1)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2 + 1))) {

        new_positions += ((coordinates._1 + 1, coordinates._2))
        if (coordinates._1 + 1 > new_boundaries._1._2) new_boundaries = ((new_boundaries._1._1, coordinates._1 + 1), new_boundaries._2)
        if (coordinates._1 + 1 < min_y) min_y = coordinates._1 + 1
        if (coordinates._1 + 1 > max_y) max_y = coordinates._1 + 1
        if (coordinates._2 < min_x) min_x = coordinates._2
        if (coordinates._2 > max_x) max_x = coordinates._2

      } else if (!positions.contains((coordinates._1 - 1, coordinates._2 - 1)) &&
        !positions.contains((coordinates._1, coordinates._2 - 1)) &&
        !positions.contains((coordinates._1 + 1, coordinates._2 - 1))) {

        new_positions += ((coordinates._1, coordinates._2 - 1))
        if (coordinates._2 - 1 < new_boundaries._2._1) new_boundaries = (new_boundaries._1, (coordinates._2 - 1, new_boundaries._2._2))
        if (coordinates._1 < min_y) min_y = coordinates._1
        if (coordinates._1 > max_y) max_y = coordinates._1
        if (coordinates._2 - 1 < min_x) min_x = coordinates._2 - 1
        if (coordinates._2 - 1 > max_x) max_x = coordinates._2 - 1

      } else {
        if (coordinates._1 < min_y) min_y = coordinates._1
        if (coordinates._1 > max_y) max_y = coordinates._1
        if (coordinates._2 < min_x) min_x = coordinates._2
        if (coordinates._2 > max_x) max_x = coordinates._2
        new_positions += coordinates
      }
    } else {
      if (coordinates._1 < min_y) min_y = coordinates._1
      if (coordinates._1 > max_y) max_y = coordinates._1
      if (coordinates._2 < min_x) min_x = coordinates._2
      if (coordinates._2 > max_x) max_x = coordinates._2
      new_positions += coordinates
    }
  }

  (new_positions, ((min_y, max_y), (min_x, max_x)))

}

def empty_tiles(filename: String, part:Int): Long  = {

  val parsing = parse_file(filename)
  val map = parsing._1
  var positions = parsing._2
  var boundaries = parsing._3
  var direction = "N"

  if (part == 1){

    for (round <- Range(1, 11)) {

      var moves: (mutable.ListBuffer[(Int, Int)], ((Int, Int), (Int, Int))) = (mutable.ListBuffer(), ((0, 0), (0, 0)))
      var positions_to_check: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer()
      val new_positions: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer()

      direction match {
        case "N" => moves = north_first(map, positions, boundaries)
          direction = "S"
        case "S" => moves = south_first(map, positions, boundaries)
          direction = "W"
        case "W" => moves = west_first(map, positions, boundaries)
          direction = "E"
        case "E" => moves = east_first(map, positions, boundaries)
          direction = "N"
      }

      positions_to_check = moves._1
      boundaries = moves._2

      for ((cord, i) <- positions_to_check.zipWithIndex) {
        if (positions_to_check.patch(i, Nil, 1).contains(cord)) {
          new_positions += positions(i)
        } else {
          new_positions += positions_to_check(i)
        }
      }

      positions = new_positions

    }

    var count = 0

    for (i <- Range(boundaries._1._1, boundaries._1._2 + 1)) {
      for (j <- Range(boundaries._2._1, boundaries._2._2 + 1)) {
        if (positions.contains((i, j))) {
        } else {
          count += 1
        }
      }
    }
    count
  } else {
    var positions_to_check: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer()
    var count = 0
    var moving: Boolean = true

    while (moving) {
      count += 1

      var moves: (mutable.ListBuffer[(Int, Int)], ((Int, Int), (Int, Int))) = (mutable.ListBuffer(), ((0, 0), (0, 0)))
      val new_positions: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer()

      direction match {
        case "N" => moves = north_first(map, positions, boundaries)
          direction = "S"
        case "S" => moves = south_first(map, positions, boundaries)
          direction = "W"
        case "W" => moves = west_first(map, positions, boundaries)
          direction = "E"
        case "E" => moves = east_first(map, positions, boundaries)
          direction = "N"
      }

      positions_to_check = moves._1
      boundaries = moves._2

      for ((cord, i) <- positions_to_check.zipWithIndex) {
        if (positions_to_check.patch(i, Nil, 1).contains(cord)) {
          new_positions += positions(i)
        } else {
          new_positions += positions_to_check(i)
        }
      }

      println(count)
      if (positions == positions_to_check) {
        println("not moving")
        moving = false
        println(positions)
        println(positions_to_check)
      } else {
        println("moving")
      }

      positions = new_positions
    }

    count
  }
}

// First and second star
println(f"Part1 (Test): empty tiles = ${empty_tiles(filename = "day23/test.txt", part = 1)}")
println(f"Part1: empty tiles = ${empty_tiles(filename = "day23/input.txt", part = 1)}")
//println(f"Part2 (Test): round without moving = ${empty_tiles(filename = "day23/test.txt", part = 2)}")
//println(f"Part2: round without moving = ${empty_tiles(filename = "day22/input.txt", part = 2)}")
