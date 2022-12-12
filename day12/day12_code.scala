import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.PriorityQueue


val letter_values = Map[String,Int]("S" -> 0, "a"->1, "b"->2, "c"->3, "d"->4, "e"->5,
  "f"->6, "g"->7, "h"->8, "i"->9, "j"->10, "k"->11, "l"->12, "m"->13, "n"->14,
  "o"->15, "p"->16, "q"->17, "r"->18, "s"->19, "t"->20, "u"->21, "v"->22,
  "w"->23, "x"->24, "y"->25, "z" -> 26, "E"->26)

def parse_map(map: ArrayBuffer[ArrayBuffer[(Int, (Int, Int), Int)]], filename:String):
(ArrayBuffer[ArrayBuffer[(Int, (Int, Int), Int)]], (Int, Int), (Int, Int)) = {

  var x_coord: Int = 0
  var y_coord: Int = 0
  var elevation: Int = 0
  var value: (Int, (Int, Int), Int) = (0, (0, 0), 0)
  var goal_coord: (Int, Int) = (0, 0)
  var initial_coord: (Int, Int) = (0, 0)


  for (line <- Source.fromFile(filename).getLines) {

    val map_row: ArrayBuffer[(Int, (Int, Int), Int)] = ArrayBuffer[(Int, (Int, Int), Int)]()
    for (char <- line) {
      elevation = letter_values(char.toString)
      value = (elevation, (y_coord, x_coord), 0)
      map_row += value
      if (char == 'E') goal_coord = (y_coord, x_coord)
      if (char == 'S') initial_coord = (y_coord, x_coord)
      x_coord += 1
    }
    y_coord += 1
    x_coord = 0
    map += map_row
  }

  (map, goal_coord, initial_coord)
}

def get_neighbors(map: ArrayBuffer[ArrayBuffer[(Int, (Int, Int), Int)]], coordinates: (Int, Int)): ArrayBuffer[(Int, Int)]={
  val neighbors = ArrayBuffer[(Int, Int)]()

  if (coordinates._1 > 0) {
    neighbors += ((coordinates._1 - 1, coordinates._2))
  }
  if (coordinates._1 < map.length - 1) {
    neighbors += ((coordinates._1 + 1, coordinates._2))
  }
  if (coordinates._2 > 0) {
    neighbors += ((coordinates._1, coordinates._2 - 1))
  }
  if (coordinates._2 < map(0).length - 1) {
    neighbors += ((coordinates._1, coordinates._2 + 1))
  }

  neighbors
}

def distance_to_goal(coordinates: (Int, Int), goal: (Int, Int)): Int = {
  (coordinates._1 - goal._1).abs + (coordinates._2 - goal._2).abs
}

def bfs(map:ArrayBuffer[ArrayBuffer[(Int, (Int, Int), Int)]], start: (Int, Int), goal: (Int, Int)): Int ={
  val visited: ArrayBuffer[(Int, Int)] = ArrayBuffer[(Int, Int)]()
  var neighbours: ArrayBuffer[(Int, Int)] = ArrayBuffer[(Int, Int)]()
  val toVisit: PriorityQueue[((Int, Int), Int, Int)] = PriorityQueue.empty(Ordering.by(-_._2))
  var checked: Int = 0
  var neighbour_high = 0
  var node_high = 0

  toVisit.enqueue((start, distance_to_goal(start, goal), 0))
  var node: ((Int, Int), Int, Int) = ((0, 0), 0, 0)

  while (toVisit.nonEmpty) {
    node = toVisit.dequeue()
    visited += node._1
    checked += 1

    if (node._1 == goal) {
      return (node._3)
    } else {

      neighbours = get_neighbors(map, node._1)
      node_high = map(node._1._1)(node._1._2)._1

      for (neighbour <- neighbours) {
        neighbour_high = map(neighbour._1)(neighbour._2)._1
        if (neighbour_high - node_high <= 1) {
          if (!visited.contains(neighbour)) {
            visited += neighbour
            toVisit.enqueue((neighbour, distance_to_goal(neighbour, goal), node._3 + 1))
          }
        }
      }

    }
  }

  return 100000000
}

def search_starts(map: ArrayBuffer[ArrayBuffer[(Int, (Int, Int), Int)]], start_value:Int): ArrayBuffer[(Int, Int)] = {
  var goal_coord: (Int, Int) = (0, 0)
  var initial_coord: (Int, Int) = (0, 0)

  val start_list: ArrayBuffer[(Int, Int)] = ArrayBuffer[(Int, Int)]()

  for (row <- map) {
    for (node <- row) {
      if (node._1 == start_value) {
        if (node._2._1 == 0 || node._2._2 == 0){
          start_list += node._2
        }
      }
    }
  }

  return start_list

}

// Use same function for both stars, the only thing that changes is the rounds and the divisor
def search_grid(filename:String, start_value: Int):Int= {

  var map: ArrayBuffer[ArrayBuffer[(Int, (Int, Int), Int)]] = ArrayBuffer[ArrayBuffer[(Int, (Int, Int), Int)]]()
  var goal_coord: (Int, Int) = (0, 0)
  var initial_coord: (Int, Int) = (0, 0)

  val return_values = parse_map(map, filename)

  map = return_values._1
  goal_coord = return_values._2
  initial_coord = return_values._3

  val goal_list: ArrayBuffer[Int] = ArrayBuffer[Int]()

  for (start <- search_starts(map, start_value)){
    goal_list += bfs(map, start, goal_coord)
  }

  return goal_list.min
  0

}

// First and second star
println(search_grid("day12/input.txt", 0))
println(search_grid("day12/input.txt", 1))
