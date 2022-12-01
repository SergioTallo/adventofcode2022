import scala.io.Source
import scala.collection.mutable.ArrayBuffer


// For the first star we only need to know the elf carrying the most calories
def find_max_calories(filename:String):Int= {
  var max_calories: Int = 0
  var temp_calories: Int = 0
  
  // Iterate through the lines of the .txt file
  for (line <- Source.fromFile(filename).getLines){
    // The calories an elf is carrying are "grouped" between blank lines, therefore a blank line means
    // that the next calories are carried by a new elf
    if (line != ""){
      temp_calories += line.toInt
    } else {
      // We don't need to store all the elves values, only the one with the maximum
      if (temp_calories > max_calories){
        max_calories = temp_calories
      }
      // new elf
      temp_calories = 0
    }
    
    // check the calories of the last elf
    if (temp_calories > max_calories) max_calories = temp_calories

  }
  return max_calories
}


// For the second star we need to know the three elves carrying the most calories
def find_three_max_calories(filename:String):Int= {
  var calories_list = ArrayBuffer[Int]()
  var temp_calories: Int = 0
  var min_three_value: Double = Double.PositiveInfinity

  // The calories an elf is carrying are "grouped" between blank lines, therefore a blank line means
  // that the next calories are carried by a new elf
  for (line <- Source.fromFile(filename).getLines){
    if (line != ""){
      temp_calories += line.toInt
    } else {
      // We only need the calories carried of the three elves carrying the higher calories
      // Therefore we store first the first three elves and update the minimum value of that three
      if (calories_list.length < 3){
        calories_list.append(temp_calories)
        if (temp_calories < min_three_value){
          min_three_value = temp_calories
        }
      } else {
        // After the list have the first three values, update the list when a elf is carrying more calories
        // than the one in the list that is carrying the less calories
        if (temp_calories > min_three_value){
          calories_list -= min_three_value.toInt
          calories_list.append(temp_calories)
          min_three_value = calories_list.min
        }
      }
      
      // new elf
      temp_calories = 0
    }
  }
  
  // Check for the calories of the last elf
  if (temp_calories > min_three_value){
    calories_list -= min_three_value.toInt
    calories_list.append(temp_calories)
    min_three_value = calories_list.min
  }

  return calories_list.sum
}

// First star
println(find_max_calories("input.txt"))
// Second star
println(find_three_max_calories("input.txt"))
