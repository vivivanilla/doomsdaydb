package doomsday.timestamp

import cats.implicits._
import cats.kernel.laws.discipline.BoundedSemilatticeTests
import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import org.scalatestplus.scalacheck.Checkers
import cats.kernel.laws.discipline.OrderTests
import cats.kernel.laws.discipline.PartialOrderTests
import cats.kernel.laws.discipline.SemilatticeTests
import cats.kernel.PartialOrder

class VectorClockTest extends AnyFunSuite with FunSuiteDiscipline with Checkers {
  import arbitraries._

  {
    implicit val semilattice = VectorClock.joinSemilattice
    implicit val order: PartialOrder[VectorClock] = VectorClock.joinSemilattice.asJoinPartialOrder(VectorClock.eq)
    checkAll("VectorClock.joinSemilattice", BoundedSemilatticeTests[VectorClock].boundedSemilattice)
    checkAll("VectorClock.joinSemilattice", PartialOrderTests[VectorClock].partialOrder)
  }

  {
    implicit val order = VectorClock.happenedBeforeOrder
    checkAll("VectorClock.happenedBeforeOrder", PartialOrderTests[VectorClock].partialOrder)
  }

  {
    implicit val order = VectorClock.lexicalOrderNodePriorityOrder
    checkAll("VectorClock.lexicalOrderNodePriorityOrder", OrderTests[VectorClock].order)
  }

  {
    implicit val semilattice = VectorClock.meetSemilattice
    checkAll("VectorClock.meetSemilattice", SemilatticeTests[VectorClock].semilattice)
  }

  test("VectorClock.update is monotonic") {
    check((clock: VectorClock, nodeId: String, time: Long) => time <= 0 || clock.update(nodeId, time) >= clock)
  }
}
