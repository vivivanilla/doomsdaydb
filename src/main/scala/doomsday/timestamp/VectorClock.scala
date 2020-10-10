package doomsday.timestamp

import cats.kernel.BoundedSemilattice
import scala.collection.decorators.MapDecorator
import cats.kernel.Semilattice
import cats.kernel.PartialOrder
import cats.kernel.Eq
import cats.kernel.Order
import cats.implicits._

case class VectorClock(time: Map[String, Long]) {

  def update(nodeId: String, newtime: Long) = VectorClock(time + (nodeId -> (time.getOrElse(nodeId, 0L) max newtime)))

  def <=(that: VectorClock) =
    this.time.forall {
      case (k, v1) =>
        that.time.get(k).exists(v1 <= _)
    }

}

object VectorClock {

  val empty = VectorClock(Map.empty)

  implicit val joinSemilattice = new BoundedSemilattice[VectorClock] {
    override def combine(x: VectorClock, y: VectorClock): VectorClock =
      VectorClock(x.time.mergeByKeyWith(y.time) {
        case (Some(a), Some(b)) => a max b
        case (Some(a), None)    => a
        case (None, Some(b))    => b
      })

    override def empty: VectorClock = VectorClock(Map.empty)
  }

  val meetSemilattice = new Semilattice[VectorClock] {
    override def combine(x: VectorClock, y: VectorClock): VectorClock =
      VectorClock(x.time.mergeByKeyWith(y.time) {
        case (Some(_), None)    => 0
        case (None, Some(_))    => 0
        case (Some(a), Some(b)) => a min b
      })
  }

  implicit val happenedBeforeOrder = new PartialOrder[VectorClock] {
    override def partialCompare(x: VectorClock, y: VectorClock): Double =
      if (x <= y) {
        if (y <= x) 0.0 else -1.0
      } else {
        if (y <= x) 1.0 else Double.NaN
      }
  }

  implicit val eq = new Eq[VectorClock] {
    override def eqv(x: VectorClock, y: VectorClock): Boolean = x.time == y.time
  }

  val lexicalOrderNodePriorityOrder = new Order[VectorClock] {
    override def compare(x: VectorClock, y: VectorClock): Int =
      if (x === y) 0
      else if (x <= y) -1
      else if (x >= y) 1
      else {
        val highestPrioNodeId = (x.time.keySet ++ y.time.keySet).toIndexedSeq.sorted.head
        val xVal = x.time.getOrElse(highestPrioNodeId, 0L)
        val yVal = y.time.getOrElse(highestPrioNodeId, 0L)
        if (xVal < yVal) -1
        else if (xVal > yVal) 1
        else 0
      }
  }
}
