package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EpidemySuite extends FunSuite {

  test("random direction") {
    val es = new EpidemySimulator
    import es.Direction._
    val d = random()
    assert (List(N, S, E, W).contains(d))
  }

  test("random room") {
    val es = new EpidemySimulator
    val room = es.Direction.randomMove((3, 3))
    assert (List((2, 3), (4, 3), (3, 2), (3, 4)).contains(room))
  }

  test("random room lower edge") {
    val es = new EpidemySimulator
    val room = es.Direction.randomMove((0, 0))
    println(room)
    assert (List((7, 0), (1, 0), (0, 7), (0, 1)).contains(room))
  }

  test("random room upper edge") {
    val es = new EpidemySimulator
    val room = es.Direction.randomMove((7, 7))
    assert (List((7, 0), (7, 6), (0, 7), (6, 7)).contains(room))
  }

  test("prevalence rate"){
    val prevalenceRate = 0.01

    val es = new EpidemySimulator
    val numInfected = es.persons.count(_.infected)

    assert(numInfected == es.SimConfig.population * prevalenceRate,
      "prevalence rate should be 0.01"
      )
  }

  test("dead person stays dead"){
    val es = new EpidemySimulator

    val chosenOne = es.persons.head
    chosenOne.infected = true
    chosenOne.sick = true
    chosenOne.dead = true
    chosenOne.immune = false

    val(row, col) = (chosenOne.row, chosenOne.col)

    val testDays = 100

    while(!es.agenda.isEmpty && es.agenda.head.time < testDays){
      es.next

      assert(chosenOne.dead == true, "Dead person should keep dead state")
      assert(chosenOne.infected == true, "Dead person keeps infected")
      assert(chosenOne.immune == false, "Dead person cannot become immune")
      assert(chosenOne.sick == true, "Dead person keeps sick")
      assert(chosenOne.col == col && chosenOne.row == row, "Dead person cannot move")
    }
  }

  test("life cycle"){
    val es = new EpidemySimulator

    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18

    val prevalenceRate = 0.01
    val transRate = 0.4
    val dieRate = 0.25

    val infectedPerson = (es.persons.find{_.infected}).get

    //before incubation time
    while(es.agenda.head.time < incubationTime){
      assert(infectedPerson.infected == true, "Infected person keeps infected in 6 days")
    	assert(infectedPerson.sick == false, "Infected person does not get sick in 6 days")
    	assert(infectedPerson.immune == false, "Infected person cannot become immune in 6 days")
    	assert(infectedPerson.dead == false, "Infected person does not die in 6 days")
    	es.next
    }

    //incubation time has passed, there should be an event for getting sick
    assert(es.agenda.head.time == incubationTime, "You should set a 'sick' event after incubation time")
    while(es.agenda.head.time == incubationTime) es.next
    assert(infectedPerson.sick == true, "Infected person should become sick after 6 days")

    //wait for dieTime
    while(es.agenda.head.time < dieTime){
    	assert(infectedPerson.infected == true, "Sick person keeps infected")
    	assert(infectedPerson.sick == true, "Sick person keeps sick before turning immune")
    	assert(infectedPerson.immune == false, "Sick person is not immune")
    	assert(infectedPerson.dead == false, "Sick person does not die before 14 infected days")
    	es.next
    }

    assert(es.agenda.head.time == dieTime, "You should set a 'die' event (decides with a probability 25% whether the person dies) after 14 days")
    while(es.agenda.head.time == dieTime) es.next

    val deadPerson = (es.persons.find(_.dead)).get
    val sickPerson = (es.persons.find(_.sick)).get

    // wait for immune time
    while(es.agenda.head.time < immuneTime){
      assert(deadPerson.infected == true, "Dead person keeps infected")
      assert(deadPerson.sick == true, "Dead person keeps sick before turning immune")
      assert(deadPerson.immune == false, "Dead person is not immune")
      assert(deadPerson.dead == true, "Dead person does not die before 14 infected days")
      assert(sickPerson.infected == true, "Sick person keeps infected")
      assert(sickPerson.sick == true, "Sick person keeps sick before turning immune")
      assert(sickPerson.immune == false, "Sick person is not immune")
      assert(sickPerson.dead == false, "Sick person does not die before 14 infected days")
      es.next
    }

    assert(es.agenda.head.time == immuneTime, "You should set an 'immune' event (decides with a probability 25% whether the person dies) after 14 days")
    while(es.agenda.head.time == immuneTime) es.next


    while(es.agenda.head.time < healTime){
      assert(deadPerson.infected == true, "Dead person keeps infected")
      assert(deadPerson.sick == true, "Dead person keeps sick before turning immune")
      assert(deadPerson.immune == false, "Dead person is not immune")
      assert(deadPerson.dead == true, "Dead person does not die before 14 infected days")
      assert(sickPerson.infected == true, "Immune person keeps infected")
      assert(sickPerson.sick == false, "Immune person keeps sick before turning immune")
      assert(sickPerson.immune == true, "Immune person is immune")
      assert(sickPerson.dead == false, "Immune person does not die before 14 infected days")
      es.next
    }

    assert(es.agenda.head.time == healTime, "You should set a 'heal' event (decides with a probability 25% whether the person dies) after 14 days")
    while(es.agenda.head.time == healTime) es.next

    // Back to start
    assert(deadPerson.infected == true, "Dead person keeps infected")
    assert(deadPerson.sick == true, "Dead person keeps sick before turning immune")
    assert(deadPerson.immune == false, "Dead person is not immune")
    assert(deadPerson.dead == true, "Dead person does not die before 14 infected days")
    assert(sickPerson.infected == false, "Immune person not infected")
    assert(sickPerson.sick == false, "Immune person not sich")
    assert(sickPerson.immune == false, "Immune person is not immune")
    assert(sickPerson.dead == false, "Immune person does not die before 14 infected days")


  }


  test("transmissibility rate"){
	  var infectedTimes = 0
	  for(i <- 0 to 100){
		  val es = new EpidemySimulator
		  val healthyPerson = (es.persons find {p => !p.infected}).get
		  es.persons.filter(p => p != healthyPerson) foreach {_.infected = true}

      while(es.agenda.head.time < 6) es.next

      infectedTimes = infectedTimes + (if(healthyPerson.infected) 1 else 0)
	  }
	  assert(infectedTimes > 0, "A person should get infected according to the transmissibility rate when he moves into a room with an infectious person")
  }
}