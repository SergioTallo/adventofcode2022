import scala.io.Source
import scala.collection.mutable.ListBuffer

class Valve (valve_name:String, flow: Int, var state: Boolean, neighbours: ListBuffer[String]) {
  var name: String = valve_name
  var flowrate:Int = flow
  var open:Boolean = state
  var neighbors_list:ListBuffer[String] = neighbours

  def openValve(): Unit = open = true
}

def parse_file(filename:String):ListBuffer[Valve] = {

  val data = Source.fromResource(filename).getLines().filter(_.nonEmpty).toSeq

  var valves = ListBuffer[Valve]()

  for (line <- data) {
    // regex for find all two characters after the word valve
    var data: ListBuffer[String] = ListBuffer()

    var b = ("""Valve (\w\w)""".r findFirstIn  line).get.split(" ")(1)
    var c = ("""rate=(\d+)""".r findFirstIn line).get.split("=")(1)

    var a = line.split("valve")(1).replace("s", "").replace(" ", "").split(",")
    var neighbours = ListBuffer[String]()
    for (i <- a) {
      neighbours += i
    }

    var valve = new Valve(b, c.toInt, state = false, neighbours)
    valves += valve
  }

  valves
}

def get_path(actual_valve: Valve, valves: ListBuffer[Valve], path: Map[String, Int], dist_to_origin: Int): Map[String, Int] ={
  var new_path = path
  if (path.isEmpty) {
    new_path += (actual_valve.name -> 0)
  }
  for (i <- actual_valve.neighbors_list) {
    if ((!new_path.contains(i)) || ((new_path(i) > dist_to_origin))) {
      new_path += (i -> (dist_to_origin + 1))
    }
  }

  for (i <- actual_valve.neighbors_list) {
    for (next <- valves.filter(_.name == i).head.neighbors_list) {
      if ((!new_path.contains(next)) || ((new_path(next) > dist_to_origin))) {
        new_path = get_path(valves.filter(_.name == i).head, valves, new_path, dist_to_origin + 1)
      }
    }
  }

  new_path
}

def get_graph(valves: ListBuffer[Valve]): Map[String, Map[String, Int]] = {

  val path = Map[String, Int]()
  var graph = Map[String, Map[String, Int]]()

  for (i <- valves) {
    graph += (i.name -> get_path(i, valves, path, dist_to_origin = 0))
  }

  graph
}

def get_actions(actual_valve:Valve, valves: ListBuffer[Valve], actual_valve_distances: Map[String, Int],
                open_valves:ListBuffer[Valve]): ListBuffer[(String, Int)] = {
  val actions = ListBuffer[(String, Int)]()
  if ((!actual_valve.open) && (actual_valve.flowrate > 0)) {
    actions += (("open", 1))
  }

  for (i <- actual_valve_distances) {
    if(!open_valves.contains(valves.filter(_.name == i._1).head) && (valves.filter(_.name == i._1).head.flowrate > 0)) {
      if (i._2 > 0) {
        actions += i
      }
    }
  }
  actions
}

def get_total_flow (valves: ListBuffer[Valve]): Int = {
  var total_flow = 0
  for (valve <- valves) {
    if (valve.open) {
      total_flow += valve.flowrate
    }
  }
  total_flow
}

def episode (actual_valve: Valve, valves: ListBuffer[Valve], graph: Map[String, Map[String, Int]],
             list_end_flows: ListBuffer[Int], action_number: Int, actual_flow: Int, open_valves:ListBuffer[Valve],
             lst_paths: ListBuffer[ListBuffer[String]], pth: ListBuffer[String]): (ListBuffer[Int], ListBuffer[ListBuffer[String]]) ={
  println("action number: " + action_number)
  println("Valve: " + actual_valve.name)
  println("episode flow: " + get_total_flow(valves))

  var list_flows = list_end_flows
  var list_paths = lst_paths
  var path = pth

  println("flow: " +  (get_total_flow(valves) + actual_flow))
  val open_valves_list = open_valves

  if (action_number == 30) {
    println((get_total_flow(valves) + actual_flow))
    list_flows += (get_total_flow(valves) + actual_flow)
    println(path)
    return (list_flows, list_paths)
  }
  val actions = get_actions(actual_valve, valves, graph(actual_valve.name), open_valves_list)

  println(actions)

  if (actions.isEmpty) {
    println("no actions")
    println(f"next ${30 - action_number} for ${(get_total_flow(valves) * (29 - action_number))}")
    list_flows += (get_total_flow(valves) * (30 - action_number)) + (get_total_flow(valves) + actual_flow)
    list_paths += path
    println(path)
    return (list_flows, list_paths)
  }
  for (action <- actions) {
    if (action._1 == "open") {
      actual_valve.openValve()
      open_valves_list += actual_valve
      path += (actual_valve.name + " open")
      var pack = episode(actual_valve, valves, graph, list_end_flows, (action_number + action._2),
        (get_total_flow(valves) + actual_flow), open_valves_list, list_paths, path)
      list_flows = pack._1
      list_paths = pack._2
    }
    else if (action._2 + action_number <= 30) {
      println("From " + actual_valve.name + " to " + action._1 + " with " + action._2 + " actions")
      path += (actual_valve.name + " to " + action._1)
      var pack = episode(valves.filter(_.name == action._1).head, valves, graph, list_end_flows,
        (action_number + action._2), ((get_total_flow(valves) * (action._2)) + actual_flow), open_valves_list, list_paths, path)
      list_flows = pack._1
      list_paths = pack._2
    }
  }

  return (list_flows, list_paths)
}

def sand(filename: String, y_coord: Int): Int  = {

  val valves = parse_file(filename)
  var graph = get_graph(valves)
  var list_end_flows = ListBuffer[Int]()
  var open_valves_list = ListBuffer[Valve]()
  var actual_valve = valves.filter(_.name == "AA").head
  var list_paths = ListBuffer[ListBuffer[String]]()
  var path = ListBuffer[String]()

  var pack = episode(actual_valve, valves, graph, list_end_flows, action_number = 1, actual_flow = 0,
    open_valves_list, list_paths, path)
  list_end_flows = pack._1
  list_paths = pack._2
  println(list_end_flows)
  println(list_paths)
  return list_end_flows.max
}

// First and second star
println(f"Part1: ${sand(filename = "day16/input.txt", y_coord = 2000000)}")
//println(f"Part2: ${sand2("day15/test.txt", y_coord = 10)}")
