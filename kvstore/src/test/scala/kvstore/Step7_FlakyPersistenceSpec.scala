package kvstore

import akka.testkit.{TestProbe, ImplicitSender, TestKit}
import akka.actor.ActorSystem
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import kvstore.Arbiter.{JoinedPrimary, JoinedSecondary, Join}
import kvstore.Persistence.{Persisted, Persist}

/**
 * Created with IntelliJ IDEA.
 * User: mlucas
 * Date: 23/12/13
 * Time: 11:39
 * To change this template use File | Settings | File Templates.
 */
class Step7_FlakyPersistenceSpec extends TestKit(ActorSystem("Step2SecondarySpec"))
  with FunSuite
  with BeforeAndAfterAll
  with ShouldMatchers
  with ImplicitSender
  with Tools {

  override def afterAll(): Unit = {
    system.shutdown()
  }

  test("case1: Insertions with flaky persistence") {
    val arbiter = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter.ref, Persistence.props(flaky = true)), "case2-primary")
    val client = session(primary)

    arbiter.expectMsg(Join)
    arbiter.send(primary, JoinedPrimary)

    (0 until 10).foreach { o =>
      client.set("k1", "v1")
      client.waitAck(o)
    }
  }

}
