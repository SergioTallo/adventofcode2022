import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

class Blueprint (id_number: Int){
  var id: Int = id_number

  var ore_robot_cost: Int = 0
  var clay_robot_cost: Int = 0
  var obsidian_robot_cost:(Int, Int) = (0,0)
  var geode_robot_cost:(Int, Int) = (0,0)

  var max_ore: Int = 0
  var max_clay: Int = 0
  var max_obsidian: Int = 0

  def set_max_robots(): Unit =  {
    max_ore = List(ore_robot_cost, clay_robot_cost, obsidian_robot_cost._1, geode_robot_cost._1).max
    max_clay = List(obsidian_robot_cost._2).max
    max_obsidian = List(geode_robot_cost._2).max
  }

  def get_id(): Int = id
}

def get_actions(blueprint: Blueprint, ressources: mutable.ListBuffer[Int], robots:mutable.ListBuffer[Int]): mutable.ListBuffer[String] = {

  println("max_ore: " + blueprint.max_ore)
  println("max_clay: " + blueprint.max_clay)
  println("max_obsidian: " + blueprint.max_obsidian)

  println("ore robot cost: " + blueprint.ore_robot_cost)
  println("clay robot cost: " + blueprint.clay_robot_cost)
  println("obsidian robot cost: " + blueprint.obsidian_robot_cost)
  println("geode robot cost: " + blueprint.geode_robot_cost)


  var actions = mutable.ListBuffer[String]()

  var geode = false
  var obsidian = false
  var clay = false
  var ore = false

  if ((ressources(0) >= blueprint.geode_robot_cost._1) && (ressources(2) >= blueprint.geode_robot_cost._2)) {
    actions += "geode"
    geode = true
  }

  if ((robots(2) < blueprint.max_obsidian) && (ressources(0) >= blueprint.obsidian_robot_cost._1) &&
    (ressources(1) >= blueprint.obsidian_robot_cost._2) && (!geode)) {
    actions += "obsidian"
    obsidian = true
  }

  if ((robots(0) < blueprint.max_ore) && (ressources(0) >= blueprint.ore_robot_cost) && (!geode) && (!obsidian)){
    actions += "ore"
    ore = true
  }

  if ((robots(1) < blueprint.max_clay) && (ressources(0) >= blueprint.clay_robot_cost) && (!geode) && (!obsidian)){
    actions += "clay"
    clay = true
  }

  if (!geode && !obsidian && !ore && !clay) {
    actions += "not spend"
  }

  actions
}

def get_ressources(ressources: mutable.ListBuffer[Int], robots: mutable.ListBuffer[Int]): mutable.ListBuffer[Int] = {
  for (i <- Range(0, robots.length)) {
    ressources(i) += robots(i) * 1
  }
  ressources
}

def minute(blueprint: Blueprint, ressources: mutable.ListBuffer[Int], robots: mutable.ListBuffer[Int], minutes: Int, best: Int): (Int, Int) = {
  println("")
  println("minute " + minutes)

  var geodes = ressources(3)
  var best_now = best

  var cost: Int = 0
  if (minutes <= 24) {
    println("minutes" + minutes)
    println("ressources" + ressources)
    println("robots" + robots)
    val actions = get_actions(blueprint, ressources, robots)
    println("actions: " + actions)
    for (action <- actions) {
      println("ressources" + ressources)
      println("robots" + robots)
      println("action: " + action + " minutes " + minutes)
      var ressources_b = ressources.clone()
      var new_robots = robots.clone()

      if (action == "ore") {
        ressources_b(0) -= blueprint.ore_robot_cost
        new_robots(0) += 1
      } else if (action == "clay") {
        ressources_b(0) -= blueprint.clay_robot_cost
        new_robots(1) += 1
      } else if (action == "obsidian") {
        ressources_b(0) -= blueprint.obsidian_robot_cost._1
        ressources_b(1) -= blueprint.obsidian_robot_cost._2
        new_robots(2) += 1
      } else if (action == "geode") {
        ressources_b(0) -= blueprint.geode_robot_cost._1
        ressources_b(2) -= blueprint.geode_robot_cost._2
        new_robots(3) += 1
      }

      println("ressources" + ressources_b)
      println("robots" + new_robots)

      ressources_b = get_ressources(ressources_b, robots)

      println("collect")
      println("ressources" + ressources_b)
      println("robots" + new_robots)

      val result = minute(blueprint, ressources_b, new_robots, minutes + 1, best_now)
      geodes = result._1
    }
  }

  println("end")
  println("robots" + robots)
  var last = get_ressources(ressources, robots)(3)
  println("ressources" + ressources)
  println("geodes: " + get_ressources(ressources, robots)(3))

  if (last > best) {
    best_now = geodes
  }
  println("best: " + best_now)

  return (last, best_now)
}

def strategy(blueprint:Blueprint): Int = {
  var ressources: mutable.ListBuffer[Int] = mutable.ListBuffer[Int](0,0,0,0)
  var robots: mutable.ListBuffer[Int] = mutable.ListBuffer[Int](1, 0, 0, 0)

  while
}

def parse_file(filename:String):(mutable.ListBuffer[Blueprint]) ={
  var rocks = mutable.Map[(Int,Int, Int),Int]()
  val data = Source.fromResource(filename).getLines().filter(_.nonEmpty).toSeq
  var blueprints = mutable.ListBuffer[Blueprint]()
  var blueprint:Blueprint = new Blueprint(0)
  var id = 0
  for (line <- data){
    if (line.contains("Blueprint")){
      val pattern = """\d+""".r
      id = pattern.findAllIn(line).toList.head.toInt
      blueprint = new Blueprint(id)
      blueprints += blueprint
    } else if (line.contains("ore robot")){
      val pattern = """\d+""".r
      val ore = pattern.findAllIn(line).toList.head.toInt
      blueprints(id -1).ore_robot_cost = ore
    } else if (line.contains("clay robot")){
      val pattern = """\d+""".r
      val clay = pattern.findAllIn(line).toList.head.toInt
      blueprints(id -1).clay_robot_cost = clay
    } else if (line.contains("obsidian robot")){
      val pattern = """\d+""".r
      val numbers = pattern.findAllIn(line).toList.map(_.toInt)
      blueprints(id -1).obsidian_robot_cost = (numbers.head, numbers(1))
    } else if (line.contains("geode robot")){
      val pattern = """\d+""".r
      val numbers = pattern.findAllIn(line).toList.map(_.toInt)
      blueprints(id -1).geode_robot_cost = (numbers.head, numbers(1))
    }
  }

  for (blueprint <- blueprints){
    blueprint.set_max_robots()
  }

  blueprints
}

def get_geodes(filename: String, part_2: Boolean): Int  = {
  println("Start")
  val blueprints = parse_file(filename)
  println("Blueprints parsed")
  for (i <- blueprints){
    var limits = i.set_max_robots()
    println("Strategy for blueprint " + i.get_id())
    println(strategy(i))
  }
  0
}

// First and second star
println(f"Part1: ${get_geodes(filename = "day19/test.txt", part_2 = false)} sides exposed")
//println(f"Part2: ${lava_rocks(filename = "day18/input.txt", part_2 = true)} sides exposed")
