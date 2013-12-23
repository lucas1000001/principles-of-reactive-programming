package kvstore

import akka.actor.{OneForOneStrategy, Cancellable, Actor, ActorRef}
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy.{Stop, Restart}

class Persistor(val persistence:ActorRef) extends Actor {

  import Persistence._
  import context.dispatcher

  // id of request to actor who made request
  var awaitingPersist = Map.empty[Long, (ActorRef, Persist, Cancellable)]

  override val supervisorStrategy = OneForOneStrategy() {
    case e: PersistenceException => Restart
  }

  def receive: Receive = {
    case ps:Persist => {
      val cancel = context.system.scheduler.scheduleOnce(1 second) {
        if (awaitingPersist.contains(ps.id)) {
          val (replyTo, _, _) = awaitingPersist(ps.id)
          replyTo ! PersistenceTimeout(ps.id)
          awaitingPersist = awaitingPersist - ps.id
        }
      }
      awaitingPersist = awaitingPersist + (ps.id -> (sender, ps, cancel))
    }
    case Persisted(key, id) => {
      if (awaitingPersist.contains(id)) {
        val (toReply, _, cancellable) = awaitingPersist(id)
        cancellable.cancel()
        awaitingPersist = awaitingPersist - id
        toReply ! Persisted(key, id)
      }
    }
  }

  context.system.scheduler.schedule(100 millis, 100 millis) {
    awaitingPersist.foreach { case (_, (_, msg, _)) =>
      persistence ! msg
    }
  }

}
