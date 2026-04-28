package ex2

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  private var battery = 100
  override def act(): Unit =
    battery -= 50
    if battery < 0 then
      println("Robot out of energy")
    else
      robot.act()

class RobotCanFail(val robot: Robot,  val failureProbability: Int) extends Robot:
  export robot.{position, direction, turn}
  private val rand = new scala.util.Random
  override def act(): Unit =
    if rand.nextInt(100) >= failureProbability then
      robot.act()
    else println("Fail")

class RobotRepeated(val robot: Robot, val rep: Int) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    for _ <- 1 to rep do
      robot.act()

@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East

  val robot2 = RobotWithBattery(SimpleRobot((0,0), Direction.North))
  robot2.act()
  robot2.turn(robot.direction.turnRight)
  robot2.act()
  robot2.act() // robot out of energy
  robot2.act() // robot out of energy

  val robot3 = RobotCanFail(SimpleRobot((0, 0), Direction.North), 50)
  robot3.act()
  robot3.turn(robot.direction.turnRight)
  robot3.act()
  robot3.act()
  robot3.act()