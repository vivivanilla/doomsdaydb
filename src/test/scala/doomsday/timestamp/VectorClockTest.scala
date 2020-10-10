package doomsday.timestamp

import cats.implicits._
import cats.kernel.laws.discipline.BoundedSemilatticeTests
import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import org.scalatestplus.scalacheck.Checkers
import cats.kernel.laws.discipline.OrderTests
import cats.kernel.laws.discipline.PartialOrderTests

class VectorClockTest extends AnyFunSuite with FunSuiteDiscipline with Checkers {
  import arbitraries._

  checkAll("VectorClock.BoundedSemigroupLaws", BoundedSemilatticeTests[VectorClock].boundedSemilattice)

  {
    implicit val order = VectorClock.happenedBeforeOrder
    checkAll("VectorClock.PartialOrderLaws", PartialOrderTests[VectorClock].partialOrder)
  }

  {
    implicit val order = VectorClock.lexicalOrderNodePriorityOrder
    checkAll("VectorClock.OrderLaws", OrderTests[VectorClock].order)
  }

  test("VectorClock.update is monotonic") {
    check((clock: VectorClock, nodeId: String, time: Long) => time <= 0 || clock.update(nodeId, time) >= clock)
  }
}
