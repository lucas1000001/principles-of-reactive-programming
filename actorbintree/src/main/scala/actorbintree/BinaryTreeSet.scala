/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue
import scala.concurrent.Future

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case GC => {
      println("GC-ing")
      val newRoot = createRoot
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
    }
    case op:Operation => root ! op
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case CopyFinished => {
      val oldRoot = root
      root = newRoot
      pendingQueue.foreach(root ! _)
      pendingQueue = Queue.empty[Operation]
      context.become(normal)
      oldRoot ! PoisonPill
      println("GC done")
    }
    case op:Operation => pendingQueue = pendingQueue.enqueue(op)
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  def contains(caller:ActorRef, id:Int, e:Int) {

    def delegate(pos:Position) {
      if (subtrees.contains(pos)) {
        subtrees(pos) ! Contains(caller, id, e)
      } else {
        caller ! ContainsResult(id, false)
      }
    }

    e match {
      case _ if (e == elem) => caller ! ContainsResult(id, !removed)
      case _ if (e < elem) => delegate(Left)
      case _ if (e > elem) => delegate(Right)
    }

  }

  def insert(caller:ActorRef, id:Int, e:Int) {

    def push(pos:Position) {
      if (subtrees.contains(pos)) {
        subtrees(pos) ! Insert(caller, id, e)
      } else {
        val child = context.actorOf(BinaryTreeNode.props(e, initiallyRemoved = false))
        subtrees = subtrees + (pos -> child)
        caller ! OperationFinished(id)
      }
    }

    e match {
      case _ if (e == elem) => {
        removed = false
        caller ! OperationFinished(id)
      }
      case _ if (e < elem) => push(Left)
      case _ if (e > elem) => push(Right)
    }

  }

  def remove(caller:ActorRef, id:Int, e:Int) {

    def delegate(pos:Position) {
      if (subtrees.contains(pos)) {
        subtrees(pos) ! Remove(caller, id, e)
      } else {
        caller ! OperationFinished(id)
      }
    }

    e match {
      case _ if (e == elem) => {
        removed = true
        caller ! OperationFinished(id)
      }
      case _ if (e < elem) => delegate(Left)
      case _ if (e > elem) => delegate(Right)
    }

  }

  def copyTo(node:ActorRef) {
    val expected = if (removed) {
      subtrees.values.toList
    } else {
      node :: subtrees.values.toList
    }
    context.become(copying(sender, expected.toSet.size))
    if (!removed) {
      node ! Insert(self, elem, elem)
    }
    subtrees.values.foreach(_ ! CopyTo(node))
  }

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Contains(req, id, e) => contains(req, id, e)
    case Insert(req, id, e) => insert(req, id, e)
    case Remove(req, id, e) => remove(req, id, e)
    case CopyTo(node) => copyTo(node)
  }


  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(replyTo:ActorRef, countDownLatch:Int): Receive = {
    case OperationFinished(_) | CopyFinished => {
      val dec = countDownLatch - 1
      if (dec == 0) {
        context.become(normal)
        replyTo ! CopyFinished
        self ! PoisonPill
      } else {
        context.become(copying(replyTo, dec))
      }
    }
  }

}
