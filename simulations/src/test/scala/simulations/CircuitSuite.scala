package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  private def assertOr(or:(Wire, Wire, Wire) => Unit, a:Boolean, b:Boolean, c:Boolean) {
    val in1, in2, out = new Wire
    or(in1, in2, out)
    in1.setSignal(a)
    in2.setSignal(b)
    run
    assert(out.getSignal === c)
  }

  test("orGate (0 | 0) => 0") {
    assertOr(orGate, false, false, false)
  }

  test("orGate (1 | 0) => 1") {
    assertOr(orGate, true, false, true)
  }

  test("orGate (0 | 1) => 1") {
    assertOr(orGate, false, true, true)
  }

  test("orGate (1 | 1) => 1") {
    assertOr(orGate, true, true, true)
  }

  test("orGate2 (0 | 0) => 0") {
    assertOr(orGate2, false, false, false)
  }

  test("orGate2 (1 | 0) => 1") {
    assertOr(orGate2, true, false, true)
  }

  test("orGate2 (0 | 1) => 1") {
    assertOr(orGate2, false, true, true)
  }

  test("orGate2 (1 | 1) => 1") {
    assertOr(orGate2, true, true, true)
  }

  test("simple demux") {
    val in, out = new Wire
    demux(in, List(), List(out))

    in.setSignal(false)
    run
    assert(out.getSignal === false)

    in.setSignal(true)
    run
    assert(out.getSignal === true)
  }
//
//  test("demux single control") {
//    val in, c, out1, out2 = new Wire
//    demux(in, List(c), List(out1, out2))
//
//    def t(inb:Boolean, c1b:Boolean, o1b:Boolean, o2b:Boolean) {
//      in.setSignal(inb)
//      c.setSignal(c1b)
//      run
//      assert(out1.getSignal === o1b)
//      assert(out2.getSignal === o2b)
//    }
//
//    //  I      C      O1     O2
//    t(false, false, false, false)
//    t(false, true,  false, false)
//    t(true,  false, true,  false)
//    t(true,  true,  false, true)
//  }
//
//  test("demux double control") {
//    val in, c1, c2, o1, o2, o3, o4 = new Wire
//    demux(in, List(c1, c2), List(o1, o2, o3, o4))
//
//    def t(inb:Boolean, c1b:Boolean, c2b:Boolean, o1b:Boolean, o2b:Boolean, o3b:Boolean, o4b:Boolean) {
//      in.setSignal(inb)
//      c1.setSignal(c1b)
//      c2.setSignal(c2b)
//      run
//      assert(o1.getSignal === o1b)
//      assert(o2.getSignal === o2b)
//      assert(o3.getSignal === o3b)
//      assert(o4.getSignal === o4b)
//    }
//
//    //  I      C1     C2     O1     O2     O3     O4
//    t(false, false, false, false, false, false, false)
//    t(false, false, true,  false, false, false, false)
//    t(false, true,  false, false, false, false, false)
//    t(false, true,  true,  false, false, false, false)
//    t(true,  false, false, true,  false, false, false)
//    t(true,  false, true,  false, true,  false, false)
//    t(true,  true,  false, false, false, true,  false)
//    t(true,  true,  true,  false, false, false, true)
//
//  }

  test("demux with 2 controls test") {
    val in, c0, c1, out1, out2,  out3, out4  = new Wire
    demux(in, List(c1, c0), List(out4, out3, out2, out1))
    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(true)
    run

    assert(out1.getSignal === false, "out1 signal")
    assert(out2.getSignal === false, "out2 signal")
    assert(out3.getSignal === true, "out3 signal")
    assert(out4.getSignal === false, "out4 signal")
  }

  test("demux medium") {
    val in, c0, c1, o0, o1, o2, o3 = new Wire
    val c = c1 :: c0 :: Nil
    val o = o3 :: o2 :: o1 :: o0 :: Nil
    demux(in, c, o)

    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)

    in.setSignal(true)
    run
    println(List(o0, o1, o2, o3).map(_.getSignal))
    assert(o0.getSignal === true, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)

    in.setSignal(true)
    c0.setSignal(true)
    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === true, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
  }

  test("demux large") {
    val in, c0, c1, c2, c3, o0, o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15 = new Wire
    val c = c3 :: c2 :: c1 :: c0 :: Nil
    val o = o15 :: o14 :: o13 :: o12 :: o11 :: o10 :: o9 :: o8 :: o7 :: o6 :: o5 :: o4 :: o3 :: o2 :: o1 :: o0 :: Nil
    demux(in, c, o)

    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
    assert(o4.getSignal === false, "o4")
    assert(o5.getSignal === false, "o5")
    assert(o6.getSignal === false, "o6")
    assert(o7.getSignal === false, "o7")
    assert(o8.getSignal === false, "o8")
    assert(o9.getSignal === false, "o9")
    assert(o10.getSignal === false, "o10")
    assert(o11.getSignal === false, "o11")
    assert(o12.getSignal === false, "o12")
    assert(o13.getSignal === false, "o13")
    assert(o14.getSignal === false, "o14")
    assert(o15.getSignal === false, "o15")

    in.setSignal(true)
    run
    assert(o0.getSignal === true, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
    assert(o4.getSignal === false, "o4")
    assert(o5.getSignal === false, "o5")
    assert(o6.getSignal === false, "o6")
    assert(o7.getSignal === false, "o7")
    assert(o8.getSignal === false, "o8")
    assert(o9.getSignal === false, "o9")
    assert(o10.getSignal === false, "o10")
    assert(o11.getSignal === false, "o11")
    assert(o12.getSignal === false, "o12")
    assert(o13.getSignal === false, "o13")
    assert(o14.getSignal === false, "o14")
    assert(o15.getSignal === false, "o15")

    in.setSignal(true)
    c0.setSignal(true)
    c3.setSignal(true)
    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
    assert(o4.getSignal === false, "o4")
    assert(o5.getSignal === false, "o5")
    assert(o6.getSignal === false, "o6")
    assert(o7.getSignal === false, "o7")
    assert(o8.getSignal === false, "o8")
    assert(o9.getSignal === true, "o9")
    assert(o10.getSignal === false, "o10")
    assert(o11.getSignal === false, "o11")
    assert(o12.getSignal === false, "o12")
    assert(o13.getSignal === false, "o13")
    assert(o14.getSignal === false, "o14")
    assert(o15.getSignal === false, "o15")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

}
