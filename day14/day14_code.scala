import scala.io.Source
import scala.collection.mutable.ListBuffer

def parse_map(filename:String, part2:Boolean): (Array[Array[String]], Int) = {

  val data = Source.fromResource(filename).getLines().toSeq
  val rocks: ListBuffer[((Int, Int), (Int,Int))] = ListBuffer()
  var coordinate_x_max: Int = 0
  var coordinate_y_max: Int = 0
  var coordinate_x_min: Int = 10000

  for (line <- data) {
    var start_coord = (0, 0)
    var end_coord = (0, 0)

    for (coord <- line.split(" -> ")) {
      end_coord = (coord.split(",")(0).toInt, coord.split(",")(1).toInt)

      if (start_coord != (0, 0)) {
        rocks += ((start_coord, end_coord))
      }

      if (end_coord._1 > coordinate_x_max) {
        coordinate_x_max = end_coord._1
      }
      if (end_coord._1 < coordinate_x_min) {
        coordinate_x_min = end_coord._1
      }
      if (end_coord._2 > coordinate_y_max) {
        coordinate_y_max = end_coord._2
      }

      start_coord = end_coord
    }
  }

  if (part2){
    coordinate_y_max += 2
    coordinate_x_max += coordinate_y_max
    coordinate_x_min -= coordinate_y_max
  }


  val sand_start = 500 - coordinate_x_min
  val map = Array.ofDim[String](coordinate_y_max +1, coordinate_x_max - coordinate_x_min +1)

  for (i <- Range(0, map.length)) {
    for (j <- Range(0, map(0).length)) {
      map(i)(j) = "."
    }
  }

  map(0)(sand_start) = "+"

  for (i <- rocks){
    // x coordinate same
    if (i._1._1 == i._2._1) {
      // Rock in y coordinate
      // First smaller than second
      if (i._1._2 < i._2._2){
        for (j <- Range(i._1._2, i._2._2 + 1)) {
          map(j)(i._1._1 - coordinate_x_min) = "#"
        }
      }
      // first bigger than second
      else if (i._1._2 > i._2._2){
        for (j <- Range(i._2._2, i._1._2 + 1)) {
          map(j)(i._1._1 - coordinate_x_min) = "#"
        }
      }
    }
    // y coordinate same
    else if (i._1._2 == i._2._2) {
      if (i._1._1 > i._2._1) {
        for (j <- Range(i._2._1, i._1._1 + 1)) {
          map(i._1._2)(j - coordinate_x_min) = "#"
        }
      } else {
        for (j <- Range(i._1._1, i._2._1 + 1)) {
          map(i._1._2)(j - coordinate_x_min) = "#"
        }
      }
    }
  }

  if (part2){
    for (j <- Range(0, map(0).length)) {
      map(map.length - 1)(j) = "#"
    }
  }

  return (map, sand_start)

}

def sand_step(map:Array[Array[String]], start: (Int, Int), part2: Boolean): Int= {
  var steps = 0
  var new_map = map.clone()
  var position = start

  if (part2){
    steps += 1
  }

  while (true){
    if ((position._2 < 0) || (position._2 > map(0).length)) {
      return steps
    } else if (position._1 == map.length){
      return steps
    } else {

      if (map(position._1)(position._2) == ".") {
        position = (position._1 + 1, position._2)
      } else {
        if ((position._2 - 1 < 0) || (position._2 + 1 == map(position._1).length)) {
          for (i <- map) {
            println(i.mkString(""))
          }
          return steps
        } else {
          if (map(position._1)(position._2 - 1) == ".") {
            position = (position._1, position._2 - 1)
          } else if (map(position._1)(position._2 + 1) == ".") {
            position = (position._1, position._2 + 1)
          } else {
            new_map(position._1 - 1)(position._2) = "o"
            if(position._1 - 1 == 0) {
              return steps
            }
            position = start
            steps += 1
          }
        }
      }
    }
  }
  steps
}

def sand(filename: String, part2: Boolean): Int  = {
  val map_parsing = parse_map(filename, part2)
  val map = map_parsing._1
  val start = (1, map_parsing._2)

  sand_step(map, start, part2)
}

// First and second star
println(f"Part1: ${sand("day14/input.txt", part2 = false)}")
println(f"Part2: ${sand("day14/input.txt", part2 = true)}")
