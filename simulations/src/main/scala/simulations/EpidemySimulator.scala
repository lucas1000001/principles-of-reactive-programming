package simulations

import math.random
import scala.util.Random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  def randomBelowNonZero(i:Int) = randomBelow(i) + 1

  def randomSampleOf[A](n:Int, ns:List[A]) = {
    Random.shuffle(ns).take(n)
  }

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val prevalenceRate = 0.01
    val moveDelay = 5
    val incubationDelay = 6
    val deathsDoorDelay = 14 - incubationDelay
    val immunityDelay = 16 - deathsDoorDelay - incubationDelay
    val healthyDelay = 18 - immunityDelay - deathsDoorDelay - incubationDelay

    val canFly = false
  }

  import SimConfig._

  val rooms = new Rooms(roomRows, roomColumns)

  val persons: List[Person] = (0 until population).map(new Person(_, rooms)).toList
  randomSampleOf((prevalenceRate * population).toInt, persons).foreach(_.caught())

  /**
   * Direction enum to enumerate the four directions
   */
  object Direction extends Enumeration {

    case class Direction(r:Int, c:Int)

    val N = Direction(-1, 0)
    val S = Direction(1, 0)
    val E = Direction(0, 1)
    val W = Direction(0, -1)

    private val bag = List(N, S, E, W)

    def random() = {
      Random.shuffle(bag).head
    }

    def randomMove(coord:(Int, Int)) = {
      val (row, col) = coord
      val Direction(rowInc, colInc) = random()
      val newRow = ((row + rowInc + roomRows) % roomRows)
      val newCol = ((col + colInc + roomColumns) % roomColumns)
      (newRow, newCol)
    }

    def randomFlight() = {
      val newRow = randomBelow(roomRows)
      val newCol = randomBelow(roomColumns)
      (newRow, newCol)
    }

  }


  class Rooms(val rows:Int, val cols:Int) {

    import scala.collection.mutable

    private val rooms = for (row <- 0 until rows) yield {
      for (col <- 0 until cols) yield { mutable.Set[Person]() }
    }

    def room(coord:(Int, Int)) = {
      val (r, c) = coord
      rooms(r)(c)
    }

    def join(coord:(Int, Int), person:Person) {
      room(coord) += person
    }

    def leave(coord:(Int, Int), person:Person) {
      room(coord) -= person
    }

    def hasVisiblyInfected(coord:(Int, Int)) = !isSafe(coord)
    
    def hasInfected(coord:(Int, Int)) = room(coord).foldLeft(false) { (anyInfected, person) => anyInfected || person.infected }

    def isSafe(coord:(Int, Int)) = room(coord).forall { p => !p.dead || !p.sick }

  }

  def P(p:Int) = {
    randomBelow(100) < p
  }

  class Person (val id: Int, val rooms:Rooms) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //schedule a move on start
    move()

    //
    // to complete with simulation logic
    //


    import Direction._

    def leave() {
      rooms.leave((row, col), this)
    }

    def becomesHealthy() {
      afterDelay(healthyDelay) {
        immune = false
        infected = false
      }
    }

    def becomesImmune() {
      afterDelay(immunityDelay) {
        sick = false
        immune = true
        becomesHealthy()
      }
    }

    def chanceOfDeath() {
      afterDelay(deathsDoorDelay) {
        val dies = P(25)
        if (dies) {
          dead = true
        } else {
          becomesImmune()
        }
      }
    }

    def becomesSick() {
      afterDelay(incubationDelay) {
        sick = true
        chanceOfDeath()
      }
    }

    def caught() {
      infected = true
      becomesSick()
    }

    def exposed() {
      val caughtIt = P(40)
      if (caughtIt && !infected && !immune)
        caught()
    }

    def join() {
      rooms.join((row, col), this)
      if (rooms.hasInfected((row, col)))
        exposed()
    }

    def walk() {
      val (r, c) = randomMove((row, col))
      if (rooms.isSafe((r, c))) {
        row = r
        col = c
      }
    }

    def fly() {
      val (r, c) = randomFlight()
      row = r
      col = c
    }

    def choose() {
      val takeFlight = P(1) && canFly
      if (takeFlight) {
        fly()
      } else {
        walk()
      }
    }

    def move() {
      val delay = randomBelowNonZero(moveDelay)
      afterDelay(delay) {
        if (!dead) {
          leave()
          choose()
          join()
          move()
        }
      }
    }

  }
}
