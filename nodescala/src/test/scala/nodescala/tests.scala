package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers._

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite with ShouldMatchers {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("A Future should map all") {
    val fs = List(Future.always(1), Future.always(2), Future.always(3))
    val actual = Future.all(fs)
    assert(Await.result(actual, 1 second) == List(1, 2, 3))
  }

  test("A Future should return any") {
    val fs = Future.any(List(Future.always(1), Future.always(2), Future.always(3)))
    assert (List(1,2,3).contains(Await.result(fs, 1 second)))
  }

  test("A Future#now should throw if not ready") {
    val f = Future.delay(2 seconds)
    try {
      f.now
      fail("Expected exception")
    } catch {
      case e:NoSuchElementException => {
        //pass
      }
    }
  }

  test("A Future#now should pass now overtime") {
    val f = Future.delay(2 seconds)
    Await.result(f, 3 seconds)
    f.now
  }

  test("A Future should be able to delay") {
    import java.util.Date
    val start = new Date()
    println("Starting")
    val f = Future.delay(2 seconds)
    val then = new Date()
    println("Awaiting ...")
    Await.result(f, 2 seconds)
    println("End")
    val end = new Date()
    assert ((then.getTime - start.getTime) < 500)
    assert ((end.getTime - start.getTime) >= 2000)
  }

  test("A Future should continue with") {
    val f = Future.always(27)
    val s  = f.continueWith { fs =>
      Await.result(fs, 1 second).toString
    }
    Await.result(s, 1 second) should be("27")
  }

  test("A Future should handle failure for continue with") {
    val f = Future.failed(new IllegalArgumentException)
    val s = f.continueWith { fs =>
      Await.result(fs, 1 second)
    }
    try {
      Await.result(s, 1 second)
      fail("Expected exception")
    } catch {
      case e:IllegalArgumentException => {
        //pass
      }
    }
  }

  test("A Future should continue") {
    val f = Future.always(27)
    val res = f.continue {
      case Success(v) => v * 2
      case Failure(ex) => throw ex
    }
    Await.result(res, 1 second) should be(54)
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      println(s"Closed $loaded with $response")
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      println(s"Awaiting ${webpage.loaded}")
      val content = Await.result(webpage.loaded.future, 1  second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




