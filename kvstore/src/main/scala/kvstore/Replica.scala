package kvstore

import akka.actor.{ Props, ActorRef, Actor }
import kvstore.Arbiter._
import akka.actor.PoisonPill
import kvstore.Replica.{Await, GetResult}
import kvstore.Persistence.{PersistenceTimeout, Persisted, Persist}
import scala.concurrent.duration._


object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  case class Await(key:String, source:ActorRef, count:Int)

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}




trait Secondary extends Actor {

  import Replica._
  import Replicator._
  import Persistence._

  var kv:Map[String, String]

  var awaiting:Map[Long, Await]

  def get(key:String, id:Long)

  var persistor:ActorRef

  var expectedSeq = 0

  def snapshot(sn:Snapshot) {

    val Snapshot(key, valueOption, seq) = sn

    def save {
      valueOption match {
        case Some(v) => kv = kv + (key -> v)
        case None => kv = kv - key
      }
      expectedSeq += 1
    }

    def persist {
      awaiting = awaiting + (seq -> Await(key, sender, 1))
      persistor ! Persist(key, valueOption, seq)
    }

    if (seq < expectedSeq) {
      sender ! SnapshotAck(key, seq)
    }
    if (seq == expectedSeq) {
      save
      persist
    }
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get(key, id) => get(key, id)
    case sn:Snapshot => snapshot(sn)
    case Persisted(_, id) => {
      val Await(key, respondTo, _) = awaiting(id)
      respondTo ! SnapshotAck(key, id)
      awaiting = awaiting - id
    }
  }

}



trait Primary extends Actor {

  import Replica._
  import Replicator._

  var kv:Map[String, String]

  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]

  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  var awaiting:Map[Long, Await]

  var persistor:ActorRef

  def get(key:String, id:Long)

  def replicate(replicator:ActorRef, rep:Replicate) {
    replicator ! rep
  }

  private def submit(key:String, value:Option[String], id:Long) {
    awaiting = awaiting + (id -> Await(key, sender, 1 + replicators.size))
    persistor ! Persist(key, value, id)
    replicators.foreach { replicator =>
      replicate(replicator, Replicate(key, value, id))
    }
  }

  def insert(key:String, value:String, id:Long) {
    kv = kv + (key -> value)
    submit(key, Some(value), id)
  }

  def remove(key:String, id:Long) {
    kv = kv - key
    submit(key, None, id)
  }

  def allocateReplicas(replicas:Set[ActorRef]) {
    val knownReplicas: Set[ActorRef] = secondaries.keys.toSet
    val toAdd = (replicas - self) &~ knownReplicas
    val toRemove = (knownReplicas - self) &~ replicas

    toAdd.foreach { replica =>
      val replicator = context.actorOf(Replicator.props(replica))
      replicators = replicators + replicator
      secondaries = secondaries + (replica -> replicator)
      kv.zip(Stream.from(0)).foreach { case ((k, v), id) =>
        replicate(replicator, Replicate(k, Some(v), id * -1))
      }
    }

    toRemove.foreach { replica =>
      val replicator = secondaries(replica)
      replicator ! PoisonPill
      secondaries = secondaries - replica
      replicators = replicators - replica
    }
  }
  
  def countDown(id:Long) {

    def doCountDown {
      val Await(key, replyTo, count) = awaiting(id)
      if (count <= 1) {
        awaiting = awaiting - id
        replyTo ! OperationAck(id)
      } else {
        awaiting = awaiting + (id -> Await(key, replyTo, count - 1))
      }
    }

    if (awaiting.contains(id)) {
      doCountDown
    }
  }

  def failed(id:Long) {

    def activeReplica(replica:ActorRef) ={
      (replicators + persistor).contains(replica)
    }

    val Await(_, replyTo, _) = awaiting(id)
    awaiting = awaiting - id

    if (activeReplica(sender)) {
      replyTo ! OperationFailed(id)
    } else {
      replyTo ! OperationAck(id)
    }

  }

  /* TODO Behavior for the leader role. */
  val leader: Receive = {
    case Get(key, id) => get(key, id)
    case Insert(key, value, id) => insert(key, value, id)
    case Remove(key, id) => remove(key, id)
    case Replicas(actors) => allocateReplicas(actors)
    case Replicated(_, id) => countDown(id)
    case Persisted(_, id) => countDown(id)
    case PersistenceTimeout(id) => failed(id)
    case ReplicationFailed(id) => failed(id)
  }

}



class Replica(val arbiter: ActorRef, val persistenceProps: Props) extends Actor with Primary
                                                                                with Secondary {

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  arbiter ! Join

  var kv = Map.empty[String, String]

  def receive = {
    case JoinedPrimary => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  def get(key:String, id:Long) {
    val valueOption = kv.get(key)
    sender ! GetResult(key, valueOption, id)
  }

  val persistence = context.actorOf(persistenceProps)

  var persistor = context.actorOf(Props(classOf[Persistor], persistence))

  var awaiting = Map.empty[Long, Await]

}
