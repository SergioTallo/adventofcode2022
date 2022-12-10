import scala.io.Source
import scala.collection.mutable.ArrayBuffer


// For the first star
def visible_trees(filename:String):Int= {
  var visible:Boolean = true
  var trees = ArrayBuffer[String]()

  for (line <- Source.fromFile(filename).getLines.filter(!_.isEmpty())){
    trees += line
  }

  var count:Int = ((trees.length * 2) + ((trees(0).length -2) * 2))

  for (i <- Range(1, (trees.length -1))){
    for (j <- Range(1, (trees(i).length -1))){
      // Same row
      visible = true
      //left
      for (k <- Range(0, j)){
        if (trees(i)(j).toInt <= trees(i)(k).toInt){
          visible = false
        }
      }

      //right
      if (visible == false){
        visible = true
        for (k <- Range(j +1, trees(i).length)){
          if (trees(i)(j).toInt <= trees(i)(k).toInt){
            visible = false
          }
        }
      }

      // Same column
      if (visible == false){
        visible = true

        //top
        for (k <- Range(0, i)){
          if (trees(i)(j).toInt <= trees(k)(j).toInt){
            visible = false
          }
        }

        //Bottom
        if (visible == false){
          visible = true
          for (k <- Range(i +1, trees.length)){
            if (trees(i)(j).toInt <= trees(k)(j).toInt){
              visible = false
            }
          }
        }
      }

      if (visible == true){
        count += 1
      }
    }
  }
  return count
}


// For the second star
def visible_trees2(filename:String):Int= {
  var visibleline:Int = 0
  var visiblelinetotal:Int = 0
  var bestplace:Int = 0
  var trees = ArrayBuffer[String]()

  for (line <- Source.fromFile(filename).getLines.filter(!_.isEmpty())){
    trees += line
  }

  var count:Int = ((trees.length * 2) + ((trees(0).length -2) * 2))


  for (i <- Range(1, (trees.length -1))){
    for (j <- Range(1, (trees(i).length -1))){
      // Same row
      //left
      visibleline = j
      for (k <- Range(0, j)){
        if (trees(i)(j).toInt <= trees(i)(k).toInt){
          visibleline = j - k
        }
      }
      visiblelinetotal = visibleline

      //right
      var visible:Boolean = true
      visibleline = 0
      for (k <- Range(j +1, trees(i).length)){
        if (visible == true){
          visibleline += 1
          if (trees(i)(j).toInt <= trees(i)(k).toInt){
            visible = false
          }
        }
      }
      if (visibleline == 0){
        visibleline = 1
      }
      visiblelinetotal *= visibleline

      // Same column
      //top
      visibleline = i
      for (k <- Range(0, i)){
        if (trees(i)(j).toInt <= trees(k)(j).toInt){
          visibleline = i - k
        }
      }

      visiblelinetotal *= visibleline

      //Bottom
      visible = true
      visibleline = 0
      for (k <- Range(i +1, trees.length)){
        if (visible == true){
          visibleline += 1
          if (trees(i)(j).toInt <= trees(k)(j).toInt){
            visible = false
          }
        }
      }
      if (visibleline == 0){
        visibleline = 1
      }

      visiblelinetotal *= visibleline


      if (visiblelinetotal > bestplace){
        bestplace = visiblelinetotal
      }
    }
  }
  return bestplace
}


// First star
println(visible_trees("input.txt"))
// Second star
println(visible_trees2("input.txt"))
