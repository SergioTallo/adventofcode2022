import scala.io.Source
import scala.collection.mutable.ArrayBuffer

val directions = Map[String,(Int, Int)]("R"->(1,0), "L"->(-1,0), "U"->(0,1), "D"->(0,-1))


// Use same function for both stars, the only thing that changes is the bodysize
def rope_moving(filename:String, bodysize:Int):Int= {
  var coordinates = scala.collection.mutable.Set[(Int, Int)]()
  var body = ArrayBuffer[(Int,Int)]()
  var direction: String = ""
  var steps: Int = 0
  var new_x: Int = 0
  var new_y: Int = 0

  for (i <- Range(0, bodysize)){
    body += ((0, 0))
  }

  for (line <- Source.fromFile(filename).getLines.filter(!_.isEmpty())){
    direction = line.split(" ")(0)
    steps = line.split(" ")(1).toInt

    for (i <- Range(0, steps)){
      body(0) = (body(0)._1 + directions(direction)._1, body(0)._2 + directions(direction)._2)

      for (i <- Range(1, body.length)){

        if (((body(i)._1 - body(i-1)._1).abs > 1) || ((body(i)._2 - body(i-1)._2).abs > 1)){
          new_x = body(i)._1
          new_y = body(i)._2
          if (body(i)._1 != body(i-1)._1){
            new_x += ((body(i-1)._1 - body(i)._1) / (body(i-1)._1 - body(i)._1).abs)
          }
          if (body(i)._2 != body(i-1)._2){
            new_y += ((body(i-1)._2 - body(i)._2) / (body(i-1)._2 - body(i)._2).abs)
          }

          body(i) = (new_x, new_y)
        }
      }
      coordinates += body(body.length -1)
    }
  }
  return coordinates.size

}

// First star bodysize of 2
println(rope_moving("day09/input.txt", 2))
// Second star bodysize of 10
println(rope_moving("day09/input.txt", 10))
