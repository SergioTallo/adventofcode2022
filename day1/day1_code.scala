import scala.io.Source
import scala.collection.mutable.ArrayBuffer


// For the first star we only need to know the elf carrying the most calories
def find_max_calories(filename:String):Int= {
  var max_calories: Int = 0
  var temp_calories: Int = 0

  for (line <- Source.fromFile(filename).getLines){
    if (line != ""){
      temp_calories += line.toInt
    } else {
      if (temp_calories > max_calories){
        max_calories = temp_calories
      }
      temp_calories = 0
    }

    if (temp_calories > max_calories) max_calories = temp_calories

  }
  return max_calories
}

def find_three_max_calories(filename:String):Int= {
  var calories_list = ArrayBuffer[Int]()
  var temp_calories: Int = 0
  var min_three_value: Double = Double.PositiveInfinity

  for (line <- Source.fromFile(filename).getLines){
    if (line != ""){
      temp_calories += line.toInt
    } else {
      if (calories_list.length < 3){
        calories_list.append(temp_calories)
        if (temp_calories < min_three_value){
          min_three_value = temp_calories
        }
      } else {
        if (temp_calories > min_three_value){
          calories_list -= min_three_value.toInt
          calories_list.append(temp_calories)
          min_three_value = calories_list.min
        }
      }

      temp_calories = 0
    }
  }

  if (temp_calories > min_three_value){
    calories_list -= min_three_value.toInt
    calories_list.append(temp_calories)
    min_three_value = calories_list.min
  }

  return calories_list.sum
}

println(find_max_calories("input.txt"))
println(find_three_max_calories("input.txt"))
