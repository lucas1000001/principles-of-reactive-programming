package simulations

import common._
import scala.annotation.tailrec
import math.pow

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val notA1, notA2, andNotA1NotA2 = new Wire
    inverter(a1, notA1)
    inverter(a2, notA2)
    andGate(notA1, notA2, andNotA1NotA2)
    inverter(andNotA1NotA2, output)
  }

  def passThroughGate(in:Wire, out:Wire) {
    def passThroughAction() {
      out.setSignal(in.getSignal)
    }
    in addAction passThroughAction
  }

  def perms(c:List[Wire]) = {
    val perms = (0 until pow(2,c.length).toInt)
    val binaryPerms = perms.map(Integer.toBinaryString(_))
    val max = binaryPerms.last.length
    val paddedPerms = binaryPerms.map(_.reverse.padTo(max, '0').reverse)
    paddedPerms.map { str =>
      str.map {
        case '0' => false
        case '1' => true
      }
    }
  }

  def invert(c:Wire) = {
    val i = new Wire
    inverter(c, i)
    i
  }

  def decoder(perms:Seq[Seq[Boolean]], c:List[Wire]) = {
    val decoders = for { perm <- perms } yield { perm.zip(c) }
    decoders.map { decoder =>
      decoder.map {
        case(true, w) => w
        case(false, w) => invert(w)
      }
    }
  }

  def andMany(wires:Seq[Wire], out:Wire):Unit = {
    wires match {
      case x :: Nil => passThroughGate(x, out)
      case x :: y :: Nil => andGate(x, y, out)
      case x :: y :: xs => {
        val w = new Wire
        andGate(x, y, w)
        andMany(w :: xs, out)
      }
    }
  }

  def wireUpDemux(in: Wire, cs: List[Wire], out: List[Wire]) {
    val ps = perms(cs)
    val decode = decoder(ps, cs)
    decode.zip(out.reverse).map { case (dec, out) =>
      val ins = dec.map { c =>
        val w = new Wire
        andGate(in, c, w)
        w
      }
      andMany(ins.toList, out)
    }  
  }
  
  def demux(in: Wire, cs: List[Wire], out: List[Wire]) {
    if (cs.isEmpty) {
      passThroughGate(in, out(0))
    } else {
      wireUpDemux(in, cs, out)
    }
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
