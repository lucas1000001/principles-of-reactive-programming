package kvstore

import akka.actor.{Cancellable, Props, Actor, ActorRef}
import scala.concurrent.duration._
import kvstore.Persistence.PersistenceTimeout

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  case class ReplicationFailed(id:Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate, Cancellable)]
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }
  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case rep:Replicate => {
      val cancel = context.system.scheduler.scheduleOnce(1 second) {
        if (acks.contains(rep.id)) {
          val (replyTo, _, _) = acks(rep.id)
          replyTo ! ReplicationFailed(rep.id)
          acks = acks - rep.id
        }
      }
      acks = acks + (nextSeq -> (sender, rep, cancel))
    }
    case SnapshotAck(key, seq) => {
      if (acks.contains(seq)) {
        val (replyTo, Replicate(key, _, id), cancellable) = acks(seq)
        cancellable.cancel()
        acks = acks - seq
        replyTo ! Replicated(key, id)
      }
    }
  }

  context.system.scheduler.schedule(100 millis, 100 millis) {
    acks.foreach { case (seq, (_, Replicate(key, valueOption, _), _)) =>
      replica ! Snapshot(key, valueOption, seq)
    }
  }

}
