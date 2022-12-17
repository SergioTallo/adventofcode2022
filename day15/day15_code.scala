import scala.io.Source
import scala.collection.mutable.ListBuffer

def l0distance(a: (Int, Int), b: (Int, Int)): Int = {
  return (a._1 - b._1).abs + (a._2 - b._2).abs
}

def parse_map(filename:String, y_coord: Int): (Map[(Int, Int), (Int, Int)], (Int, Int)) = {

  val data = Source.fromResource(filename).getLines().toSeq
  var sensor_beacon = Map[(Int, Int), (Int, Int)]()
  var max_dist_to_beacon = 0
  var min_x_ = 10000000
  var max_x_ = 0

  var count = 0

  for (line <- data) {
    var s = (0, 0)
    var b = (0, 0)

    for (i <- line.split(":")) {
      var x = 0
      var y = 0
      if (i.contains("Sensor")) {
        var temp_ = i.replace("Sensor at ", "")
        for (j <- temp_.split(", ")) {
          if (j.contains("x")) {
            x = j.replace("x=", "").toInt
          } else if (j.contains("y")) {
            y = j.replace("y=", "").toInt
          }
        }

        s = (x, y)

        if (x > max_x_) {
          max_x_ = x
        }
        if (x < min_x_) {
          min_x_ = x
        }
      } else {
        var temp_ = i.replace(" closest beacon is at ", "")
        for (j <- temp_.split(", ")) {
          if (j.contains("x")) {
            x = j.replace("x=", "").toInt
          } else if (j.contains("y")) {
            y = j.replace("y=", "").toInt
          }
        }
        b = (x, y)
      }
    }

    var distance_sensor_beacon = l0distance(s, b)

    if (distance_sensor_beacon > max_dist_to_beacon) {
      max_dist_to_beacon = distance_sensor_beacon
    }

    if (s._2 + distance_sensor_beacon > y_coord) {
      if (s._2 - distance_sensor_beacon < y_coord) {

        sensor_beacon += (s -> b)
        count += 1
      }
    }

  }

  return (sensor_beacon, (min_x_ - max_dist_to_beacon, max_x_ + max_dist_to_beacon))

}

def sand(filename: String, y_coord: Int): Int  = {
  val sensor_beacon = parse_map(filename, y_coord)

  var count = 0
  var check = false

  for (i <- Range(sensor_beacon._2._1, sensor_beacon._2._2)) {
    check = false
    for (sensor <- sensor_beacon._1) {
      var distance_sensor_beacon = l0distance(sensor._1, sensor._2)
      var distance_sensor_to_point = l0distance((i, y_coord), sensor._1)
      if (distance_sensor_to_point <= distance_sensor_beacon) {
        if (!check) {
          if ((i, y_coord) != sensor._1) {
            if ((i, y_coord) != sensor._2) {
              count += 1
              check = true
            } else {
              check = true
            }
          }else {
            check = true
          }
        }
      }
    }
  }

  return count

}

def parse_map2(filename:String, y_coord: Int): (Map[(Int, Int), ((Int, Int), Int)], (Int, Int)) = {

  val data = Source.fromResource(filename).getLines().toSeq
  var sensor_beacon = Map[(Int, Int), ((Int, Int), Int)]()
  var max_dist_to_beacon = 0
  var min_x_ = 10000000
  var max_x_ = 0

  var count = 0

  for (line <- data) {
    var s = (0, 0)
    var b = (0, 0)

    for (i <- line.split(":")) {
      var x = 0
      var y = 0
      if (i.contains("Sensor")) {
        var temp_ = i.replace("Sensor at ", "")
        for (j <- temp_.split(", ")) {
          if (j.contains("x")) {
            x = j.replace("x=", "").toInt
          } else if (j.contains("y")) {
            y = j.replace("y=", "").toInt
          }
        }

        s = (x, y)

        if (x > max_x_) {
          max_x_ = x
        }
        if (x < min_x_) {
          min_x_ = x
        }
      } else {
        var temp_ = i.replace(" closest beacon is at ", "")
        for (j <- temp_.split(", ")) {
          if (j.contains("x")) {
            x = j.replace("x=", "").toInt
          } else if (j.contains("y")) {
            y = j.replace("y=", "").toInt
          }
        }
        b = (x, y)
      }
    }

    var distance_sensor_beacon = l0distance(s, b)

    if (distance_sensor_beacon > max_dist_to_beacon) {
      max_dist_to_beacon = distance_sensor_beacon
    }

    sensor_beacon += (s -> (b, distance_sensor_beacon))

  }

  return (sensor_beacon, (min_x_ - max_dist_to_beacon, max_x_ + max_dist_to_beacon))

}

def sand2(filename: String, y_coord: Int): Int  = {
  val sensor_beacon = parse_map2(filename, y_coord)

  var break = false

  for (i <- sensor_beacon._1){
    var sensor_x = i._1._1
    var sensor_y = i._1._2

    println("sensor_x: " + sensor_x + " sensor_y: " + sensor_y + " beacon_x: " + i._2._1._1 + " beacon_y: " + i._2._1._2 + " distance: " + i._2._2)
    for (x <- Range(0, y_coord)) {
      for (y <- List(sensor_y + (i._2._2 - (x - sensor_x).abs +1),sensor_y - (i._2._2 - (x - sensor_x).abs) +1)) {
        if (y <= y_coord) {
          for (j <- sensor_beacon._1) {
            if (l0distance((x, y), j._1) <= j._2._2) {
              break = true
            }
          }
          if (!break) {
            println (x, y)
            return x * y_coord + y
          } else {
            break = false
          }
        }
      }
    }
  }

  return 0

}

// First and second star
//println(f"Part1: ${sand(filename = "day15/input.txt", y_coord = 2000000)}")
println(f"Part2: ${sand2("day15/test.txt", y_coord = 10)}")
