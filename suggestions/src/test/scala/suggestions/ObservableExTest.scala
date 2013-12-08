package suggestions

import org.scalatest.{FunSuite, FlatSpec}
import org.scalatest.matchers.ShouldMatchers
import scala.concurrent.{ExecutionContext, Future}
import suggestions.observablex.ObservableEx
import ExecutionContext.Implicits.global

/**
 * Created with IntelliJ IDEA.
 * User: mlucas
 * Date: 06/12/13
 * Time: 16:41
 * To change this template use File | Settings | File Templates.
 */
class ObservableExTest extends FunSuite with ShouldMatchers {

  test("Future to observable success") {
    val obs = ObservableEx(
      Future {
        42
      }
    )
    var actual = 0
    val sub = obs subscribe {
      actual = _
    }

    actual should be(42)
  }

  test("Future to observable failure") {
    val obs = ObservableEx(
      Future {
        throw new RuntimeException()
      }
    )
    val sub  = obs onErrorReturn { _ =>
      42
    }

    var actual = 0
    sub subscribe {
      actual = _
    }

    actual should be(42)
  }

}
