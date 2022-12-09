import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map


// For the first star
def find_message(filename:String):Int= {
  var size: Int = 0
  var read_size: Boolean = false
  var directory_sizes = Map[String,Int]()
  var current_path:String = "/root"
  var current_directory: String = "root"

  // Iterate through the lines of the .txt file
  for (line <- Source.fromFile(filename).getLines.filter(!_.isEmpty())){

    if ((read_size == true) && !(line(0) == "$"(0))){

      if ("\\d+".r.findFirstIn(line) != None){
        if (read_size == false){
          read_size = true
        }
        size += "\\d+".r.findFirstIn(line).get.toInt
      }

    } else {
      if (line(0) == "$"(0)){

        if (read_size == true){
          directory_sizes += (current_path -> 0)

          var temp_path = current_path

          while(temp_path != ""){
            directory_sizes(temp_path) = size + directory_sizes(temp_path)
            var temp_ = "/" + temp_path.split("/").last
            temp_path = temp_path.dropRight(temp_.length)
          }

          size = 0
          read_size = false
        }

        if (line.startsWith("$ dir")){
          read_size = true
        } else if (line.startsWith("$ cd ..")){
          var temp = "/" + current_directory
          current_path = current_path.replace(temp, "")
          current_directory = current_path.split("/").last
          if (current_path == ""){
            current_path = "/root"
          }
        } else if (line.startsWith("$ cd /")) {
          current_path = "/root"
          current_directory = "root"
        } else if (line.startsWith("$ cd")) {
          var directory = "cd (.*)".r.findFirstIn(line).get.split(" ")(1)
          current_path = current_path + "/" + directory
          current_directory = directory
        } else if (line.startsWith("$ ls")){
          read_size = true
        }
      }
    }
  }

  if (read_size == true){
    directory_sizes += (current_path -> 0)

    var temp_path = current_path

    while(temp_path != ""){
      directory_sizes(temp_path) = size + directory_sizes(temp_path)
      var temp_ = "/" + temp_path.split("/").last
      temp_path = temp_path.dropRight(temp_.length)
    }

    size = 0
    read_size = false
  }

  var sizetodelete: Int = 0
  for (i <- directory_sizes.keys){
    if (directory_sizes(i) <= 100000){
      sizetodelete += directory_sizes(i)
    }
  }

  return sizetodelete
}

// For the second star
def find_message2(filename:String):Int= {
  var size: Int = 0
  var read_size: Boolean = false
  var directory_sizes = Map[String,Int]()
  var current_path:String = "/root"
  var current_directory: String = "root"

  // Iterate through the lines of the .txt file
  for (line <- Source.fromFile(filename).getLines.filter(!_.isEmpty())){

    if ((read_size == true) && !(line(0) == "$"(0))){

      if ("\\d+".r.findFirstIn(line) != None){
        if (read_size == false){
          read_size = true
        }
        size += "\\d+".r.findFirstIn(line).get.toInt
      }

    } else {
      if (line(0) == "$"(0)){
        if (read_size == true){
          directory_sizes += (current_path -> 0)

          var temp_path = current_path

          while(temp_path != ""){
            directory_sizes(temp_path) = size + directory_sizes(temp_path)
            var temp_ = "/" + temp_path.split("/").last
            temp_path = temp_path.dropRight(temp_.length)
          }

          size = 0
          read_size = false
        }

        if (line.startsWith("$ dir")){
          read_size = true
        } else if (line.startsWith("$ cd ..")){
          var temp = "/" + current_directory
          current_path = current_path.replace(temp, "")
          current_directory = current_path.split("/").last
          if (current_path == ""){
            current_path = "/root"
          }
        } else if (line.startsWith("$ cd /")) {
          current_path = "/root"
          current_directory = "root"
        } else if (line.startsWith("$ cd")) {
          var directory = "cd (.*)".r.findFirstIn(line).get.split(" ")(1)
          current_path = current_path + "/" + directory
          current_directory = directory
        } else if (line.startsWith("$ ls")){
          read_size = true
        }
      }
    }
  }

  if (read_size == true){
    directory_sizes += (current_path -> 0)

    var temp_path = current_path

    while(temp_path != ""){
      directory_sizes(temp_path) = size + directory_sizes(temp_path)
      var temp_ = "/" + temp_path.split("/").last
      temp_path = temp_path.dropRight(temp_.length)
    }

    size = 0
    read_size = false
  }

  var sizes = ArrayBuffer[Int]()

  var current_space = 70000000 - directory_sizes("/root")

  for (i <- directory_sizes.keys){
    if ((current_space + directory_sizes(i)) >= 30000000){
      sizes += directory_sizes(i)
    }
  }

  return sizes.min
}


// First star
println(find_message("input.txt"))
// Second star
println(find_message2("input.txt"))
