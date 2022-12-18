import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

def parse_file(filename:String):(mutable.Map[(Int,Int, Int),Int], Int, Int, Int, Int, Int, Int) ={
  var rocks = mutable.Map[(Int,Int, Int),Int]()
  val data = Source.fromResource(filename).getLines().filter(_.nonEmpty).toSeq

  var min_x = 1000
  var max_x = 0
  var min_y = 1000
  var max_y = 0
  var min_z = 1000
  var max_z = 0

  for (pair <- data){
    var rock = pair.split(",")
    rocks += ((rock(0).toInt, rock(1).toInt, rock(2).toInt) -> 6)

    if (rock(0).toInt < min_x) min_x = rock(0).toInt
    if (rock(0).toInt > max_x) max_x = rock(0).toInt
    if (rock(1).toInt < min_y) min_y = rock(1).toInt
    if (rock(1).toInt > max_y) max_y = rock(1).toInt
    if (rock(2).toInt < min_z) min_z = rock(2).toInt
    if (rock(2).toInt > max_z) max_z = rock(2).toInt

  }

  (rocks, min_x, max_x, min_y, max_y, min_z, max_z)
}

def check_adjacent(rocks: mutable.Map[(Int,Int, Int),Int], rock: (Int,Int, Int)):Int = {
  var count = 0

  val offsets: List[(Int, Int, Int)] = List((0,0,1), (0,0,-1), (0,1,0), (0,-1,0), (1,0,0), (-1,0,0))

  for (offset <- offsets){
    if (rocks.contains((rock._1 + offset._1, rock._2 + offset._2, rock._3 + offset._3))){
      count += 1
    }
  }

  count
}

def check_adjacent_part2(lava: ArrayBuffer[(Int,Int, Int)], rock: (Int,Int, Int)):Int = {
  var count = 0

  val offsets: List[(Int, Int, Int)] = List((0,0,1), (0,0,-1), (0,1,0), (0,-1,0), (1,0,0), (-1,0,0))

  for (offset <- offsets){
    if (!lava.contains((rock._1 + offset._1, rock._2 + offset._2, rock._3 + offset._3))){
      count += 1
    }
  }

  count
}

def part2(rocks: mutable.Map[(Int,Int, Int),Int] ,bounds: ((Int, Int), (Int, Int), (Int, Int))): Int ={

  var water: ArrayBuffer[(Int, Int, Int)] = ArrayBuffer[(Int,Int, Int)]()
  var lava: ArrayBuffer[(Int, Int, Int)] = ArrayBuffer[(Int,Int, Int)]()
  var water_queue: mutable.Queue[(Int,Int, Int)] = mutable.Queue[(Int, Int, Int)]()
  val offsets: List[(Int, Int, Int)] = List((0,0,1), (0,0,-1), (0,1,0), (0,-1,0), (1,0,0), (-1,0,0))
  var min_bound_x = bounds._1._1
  var max_bound_x = bounds._1._2
  var min_bound_y = bounds._2._1
  var max_bound_y = bounds._2._2
  var min_bound_z = bounds._3._1
  var max_bound_z = bounds._3._2

  var min_coord = (min_bound_x, min_bound_y, min_bound_z)
  water_queue += min_coord

  while (water_queue.nonEmpty){
    var possible_water_coord = water_queue.dequeue()

    if (!water.contains(possible_water_coord)){
      water += possible_water_coord

      for (offset <- offsets){
        var possible_lava_coord = (possible_water_coord._1 + offset._1, possible_water_coord._2 + offset._2,
          possible_water_coord._3 + offset._3)

        if (((min_bound_x <= possible_lava_coord._1) && (possible_lava_coord._1 <= max_bound_x)) &&
          ((min_bound_y <= possible_lava_coord._2) && (possible_lava_coord._2 <=max_bound_y)) &&
          ((min_bound_z <= possible_lava_coord._3) && (possible_lava_coord._3 <= max_bound_z))){
          if (!rocks.contains(possible_lava_coord)){
            water_queue += possible_lava_coord
          }
        }
      }
    }
  }

  for (i <- Range(min_bound_x, max_bound_x +1)){
    for (j <- Range(min_bound_y, max_bound_y +1)){
      for (k <- Range(min_bound_z, max_bound_z +1)){
        if (!water.contains((i,j,k))){
          var lava_coord = (i,j,k)
          lava += lava_coord
        }
      }
    }
  }

  var count = 0

  for (rock <- lava){
    count += check_adjacent_part2(lava, rock)
  }

  return count
}

def lava_rocks(filename: String, part_2: Boolean): Int  = {

  val parser = parse_file(filename)

  var rocks = parser._1

  for (rock <- rocks){
    var adjacents = check_adjacent(rocks, rock._1)
    rocks(rock._1) -= adjacents
  }

  var sides = 0

  for (rock <- rocks){
    sides += rock._2
  }

  if(!part_2){
    return sides
  } else {
    var bounds = ((parser._2, parser._3), (parser._4, parser._5), (parser._6, parser._7))
    sides = part2(rocks, bounds)
    return sides
  }
}

// First and second star
println(f"Part1: ${lava_rocks(filename = "day18/input.txt", part_2 = false)} sides exposed")
println(f"Part2: ${lava_rocks(filename = "day18/input.txt", part_2 = true)} sides exposed")
