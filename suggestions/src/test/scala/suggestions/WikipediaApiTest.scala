package suggestions



import language.postfixOps
import scala.concurrent._
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import rx.lang.scala.subscriptions.Subscription
import rx.lang.scala.subjects.ReplaySubject
import org.scalatest.matchers.ShouldMatchers


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite with ShouldMatchers {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApp should be able to recover from errors") {
    val channel = ReplaySubject[Int]()
    val recovered = channel.recovered
    val trys = mutable.ListBuffer[Try[Int]]()
    recovered.subscribe { term =>
      trys append term
    }
    channel.onNext(0)
    channel.onNext(1)
    val ex = new Exception("Argh!")
    channel.onError(ex)
    channel.onNext(2)
    trys should be(List(Success(0), Success(1), Failure(ex)))
  }

  test("WikipediaApp should be able to complete on timeout") {
    val channel = ReplaySubject[Int]()
    val obs = channel.timedOut(1)
    val nums = mutable.ListBuffer[Int]()
    obs.subscribe { n =>
      nums append n
    }
    channel.onNext(0)
    channel.onNext(1)
    Thread.sleep(1000)
    channel.onNext(2)
    nums should be (List(0,1))
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }
}