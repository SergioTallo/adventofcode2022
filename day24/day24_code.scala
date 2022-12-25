import scala.io.Source
import scala.collection.mutable

val directions = mutable.Map[Char, (Int, Int)]('>' -> (0, 1), '<' -> (0, -1), '^' -> (-1, 0), 'v' -> (1,0))
val directions_rev = mutable.Map[(Int, Int), Char]((0, 1) -> '>', (0, -1) -> '<', (-1, 0) -> '^', (1, 0) -> 'v')

def parse_file(filename:String):(mutable.Map[Int, ((Int, Int), (Int, Int))], (Int,Int), ((Int,Int),(Int,Int)),
  (Int, Int)) = {

  val blizzards: mutable.Map[Int, ((Int, Int), (Int, Int))] = mutable.Map()
  var last_line: String = ""

  var blizzard_count = 0
  var start_coord = (0, 0)
  val lines = Source.fromResource(filename).getLines().filter(_.nonEmpty).toSeq
  val map_boundaries = ((0,lines.length -1), (0, lines.head.length -1))
  var goal_position: (Int, Int) = (0, 0)

  for (j <- Range(1, lines.head.length)) {
    if (lines.head(j) == '.') {
      start_coord = (0, j)
    }
  }

  for ((line, i) <- lines.zipWithIndex) {
    for ((square, j) <- line.zipWithIndex) {
      if ((square != '.') && (square!= '#')){
        blizzards += blizzard_count -> ((i, j), directions(square))
        blizzard_count += 1
      }
    }
    if (i == lines.length -1){
      last_line = line
    }
  }

  for (j <- Range(1, lines.last.length)) {
    if (lines.last(j) == '.') {
      goal_position = (lines.length -1, j)
    }
  }

  (blizzards, start_coord, map_boundaries, goal_position)

}

def print_map (boundaries: ((Int, Int), (Int,Int)), blizzards: mutable.ListBuffer[((Int, Int), (Int, Int))],
               start_position: (Int, Int), actual_position: (Int, Int), goal_position: (Int, Int)): Unit ={

  println("blizzards " + blizzards)

  val map = Array.ofDim[Char](boundaries._1._2 +1, boundaries._2._2+1)

  for (j <- Range(0, map(0).length)) {
    map(0)(j) = '#'
  }

  for (j <- Range(0, map(map.length -1).length)) {
    map(map.length -1)(j) = '#'
  }

  for (i <- Range(1, map.length -1)){
    map(i)(0) = '#'
    map(i)(map(i).length -1) = '#'
    for (j <- Range(1, map(i).length -1)) {
      map(i)(j) = '.'
    }
  }

  for (coord <- blizzards){
    if (map(coord._1._1)(coord._1._2) == '.'){
      map(coord._1._1)(coord._1._2) = directions_rev(coord._2)
    } else {
      map(coord._1._1)(coord._1._2) = '2'
    }
  }

  map(start_position._1)(start_position._2) = '.'
  map(goal_position._1)(goal_position._2) = '.'
  map(actual_position._1)(actual_position._2) = 'E'


  for (i <- Range(0, map.length)){
    println(map(i).mkString("Array(", ", ", ")"))
  }
}

def move_free (blizzards: mutable.ListBuffer[(Int, Int)], boundaries: ((Int, Int), (Int,Int)),
                actual_position: (Int, Int), goal_position: (Int, Int)): mutable.ListBuffer[(Int, Int)] = {

  var possible_moves: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer()

  val s: (Int, Int) = (actual_position._1 + 1, actual_position._2)

  if (s == goal_position) {
    possible_moves.append(s)
    return possible_moves
  }

  val n: (Int, Int) = (actual_position._1 -1, actual_position._2)

  if (n == goal_position) {
    possible_moves.append(n)
    return possible_moves
  }

  val e: (Int, Int) = (actual_position._1, actual_position._2 + 1)
  if ((e._2 != boundaries._2._2) && !blizzards.contains(e)) {
    possible_moves.append(e)
  }

  if ((s._1 != boundaries._1._2) && !blizzards.contains(s)) {
    possible_moves.append(s)
  }

  if ((n._1 != 0) && !blizzards.contains(n)){
    possible_moves.append(n)
  }

  val w: (Int, Int) = (actual_position._1, actual_position._2 -1)
  if ((w._2 != 0) && !blizzards.contains(w)) {
    possible_moves.append(w)
  }

  if (possible_moves.isEmpty){
    possible_moves.append(actual_position)
  }

  possible_moves

}

def blizzards_move(blizzards: mutable.Map[Int, ((Int, Int), (Int, Int))], boundaries: ((Int, Int), (Int, Int))):
  mutable.Map[Int, ((Int, Int), (Int, Int))] ={

  val new_blizzards = blizzards.clone()

  for (blizzard <- blizzards){
    var new_coord = (blizzard._2._1._1 + blizzard._2._2._1, blizzard._2._1._2 + blizzard._2._2._2)

    if (new_coord._1 == 0){
      // Out bounds up
      new_coord = (boundaries._1._2 -1, new_coord._2)
    } else if (new_coord._1 == boundaries._1._2){
      //Out bounds down
      new_coord = (1, new_coord._2)
    } else if (new_coord._2 == 0){
      //Out bounds left
      new_coord = (new_coord._1, boundaries._2._2 -1)
    } else if (new_coord._2 == boundaries._2._2){
      // Out bounds right
      new_coord = (new_coord._1, 1)
    }

    new_blizzards(blizzard._1) = (new_coord, blizzard._2._2)
  }

  new_blizzards
}

def travel (blizzards: mutable.Map[Int, ((Int, Int), (Int, Int))], boundaries: ((Int, Int), (Int, Int)),
            start: (Int, Int), end: (Int, Int)): (mutable.Map[Int, ((Int, Int), (Int, Int))], Int) = {

  var minute = 0
  var new_blizzards = blizzards.clone()

  var blizzard_queue: mutable.Queue[(Int, Int)] = mutable.Queue(start)

  while (true) {
    var possible_moves: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer()
    val list_blizzards: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer()
    val neue_blizzard_queue: mutable.Queue[(Int, Int)] = mutable.Queue()
    println("minute " + minute)
    println("blizzard_queue " + blizzard_queue)
    minute += 1

    new_blizzards = blizzards_move(new_blizzards, boundaries)

    for (i <- new_blizzards) {
      list_blizzards.append(new_blizzards(i._1)._1)
    }

    for (actual_position <- blizzard_queue) {
      possible_moves = move_free(list_blizzards, boundaries, actual_position, end)

      for (move <- possible_moves) {
        if (move == end) return (new_blizzards, minute)
        if (!neue_blizzard_queue.contains(move) && move != start) {
          neue_blizzard_queue.enqueue(move)
        }
      }
    }
    blizzard_queue = neue_blizzard_queue
  }

  (new_blizzards, minute)

}

def through_blizzard (filename: String, part:Int): Int  = {

  val parsing = parse_file(filename)
  var blizzards = parsing._1
  val start_position = parsing._2
  val boundaries = parsing._3
  val goal_position = parsing._4
  var minute = 0
  var goal_found = false

  if (part == 1){
    var result = travel(blizzards, boundaries, start_position, goal_position)
    blizzards = result._1
    minute = result._2
    minute
  } else {
    var total_minutes = 0
    var result = travel(blizzards, boundaries, start_position, goal_position)
    blizzards = result._1
    minute = result._2
    println("minute " + minute)
    total_minutes += minute

    result = travel(blizzards, boundaries, goal_position, start_position)
    blizzards = result._1
    minute = result._2
    println("minute " + minute)
    total_minutes += minute

    result = travel(blizzards, boundaries, start_position, goal_position)
    blizzards = result._1
    minute = result._2
    println("minute " + minute)
    total_minutes += minute

    return total_minutes
  }

}

// First and second star
println("")
println(f"Part1 (Test): goal reached in minute = ${through_blizzard(filename = "day24/test.txt", part = 1)}")
//println(f"Part1: goal reached in minute = ${through_blizzard(filename = "day24/input.txt", part = 1)}")
//println(f"Part2 (Test): goal reached in minute = ${through_blizzard(filename = "day24/test.txt", part = 2)}")
//println(f"Part2: goal reached in minute = ${through_blizzard(filename = "day25/input.txt", part = 2)}")
