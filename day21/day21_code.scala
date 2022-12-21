import scala.io.Source
import scala.collection.mutable

class Monkey(name_value:String, number_value:Long, op_value: String, op_monkeys: (String, String)){
  var value: Long = number_value
  var name: String = name_value
  var operation: String = op_value
  var operation_monkeys: (String, String) = op_monkeys
}

def parse_file(filename:String):mutable.ListBuffer[Monkey] = {

  val monkey_list = mutable.ListBuffer[Monkey]()

  for (line <- Source.fromResource(filename).getLines().filter(_.nonEmpty).toSeq) {

    var monkey_name = ""
    var operation = "None"
    var operation_monkeys = ("None", "None")

    val param = line.split(":")

    monkey_name = param(0)
    var number = 0.toLong

    if (param(1).trim.toLongOption.isDefined) {
      number = param(1).trim.toLong

    } else {

      // search for +, -, / or * in string and store in variable
      operation = param(1).trim.filter(c => c == '+' || c == '-' || c == '/' || c == '*')
      // split string by +, -, / or * and store in variable
      val operation_monkey_names = param(1).trim.split(operation(0))
      // create tuple of monkeys
      operation_monkeys = (operation_monkey_names(0).trim, operation_monkey_names(1).trim)
    }

    monkey_list += new Monkey(monkey_name, number, operation, operation_monkeys)
  }

  monkey_list
}

def get_value(monkey_name: String, monkey_list: mutable.ListBuffer[Monkey]): Long = {

  val monkey = monkey_list.find(_.name == monkey_name).get

  if (monkey.value == 0){

    monkey.operation match {
      case "+" => (get_value(monkey.operation_monkeys._1, monkey_list) + get_value(monkey.operation_monkeys._2, monkey_list))
      case "*" => {
        var value1 = get_value(monkey.operation_monkeys._1, monkey_list)
        var value2 = get_value(monkey.operation_monkeys._2, monkey_list)

        if (value1 == 0) value1 = 1
        if (value2 == 0) value2 = 1

        value1 * value2
      }
      case "/" => {
        var value1 = get_value(monkey.operation_monkeys._1, monkey_list)
        var value2 = get_value(monkey.operation_monkeys._2, monkey_list)

        if (value1 == 0) value1 = 1
        if (value2 == 0) value2 = 1

        value1 / value2
      }
      case "-" => (get_value(monkey.operation_monkeys._1, monkey_list) - get_value(monkey.operation_monkeys._2, monkey_list))
    }

  } else {
    monkey.value
  }
}

def sign_func(a: Long, b: Long): Long = {
  if (a > b) 1
  else if (a < b) -1
  else 0
}

def decrypt(filename: String, part:Int): Long  = {

  val monkeys = parse_file(filename)
  if (part == 1) {
    get_value("root", monkeys)
  } else {
    val monkey = monkeys.find(_.name == "root").get
    val human = monkeys.find(_.name == "humn").get
    var result_1: Long = get_value(monkey.operation_monkeys._1, monkeys)
    var result_2: Long = get_value(monkey.operation_monkeys._2, monkeys)
    var initial_sign = sign_func(result_1, result_2)

    var check_space = (1L, 9999999999999L)
    var mid:Long = 0

    while (result_1 != result_2){

      mid = ((check_space._1 + check_space._2) / 2).toLong
      human.value = mid

      result_1 = get_value(monkey.operation_monkeys._1, monkeys)
      result_2 = get_value(monkey.operation_monkeys._2, monkeys)

      var sign_mid = sign_func(result_1, result_2)

      if (sign_mid == initial_sign) {
        check_space = (mid, check_space._2)
      } else {
        check_space = (check_space._1, mid)
      }
    }

    human.value
  }
}

// First and second star
println(f"Part1 (Test): value of root = ${decrypt(filename = "day21/test.txt", part = 1)}")
println(f"Part1: value of root = ${decrypt(filename = "day21/input.txt", part = 1)}")
println(f"Part2 (Test): value of humn = ${decrypt(filename = "day21/test.txt", part = 2)}")
println(f"Part2: value of humn = ${decrypt(filename = "day21/input.txt", part = 2)}")
