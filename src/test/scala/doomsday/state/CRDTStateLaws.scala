package doomsday.state

import cats.kernel.laws._
import doomsday.timestamp.VectorClock

trait CRDTStateLaws[A] extends SemilatticeLaws[A] {

  override implicit def S: CRDTState[A]

  def cleanDistributivity(x: A, y: A, t: VectorClock): IsEq[A] =
    S.combine(S.clean(x, t), S.clean(y, t)) <-> S.clean(S.combine(x, y), t)

  def rollbackDistributivity(x: A, y: A, t: VectorClock): IsEq[A] =
    S.combine(S.rollback(x, t), S.rollback(y, t)) <-> S.rollback(S.combine(x, y), t)
}

object CRDTStateLaws {
  def apply[A](implicit ev: CRDTState[A]): CRDTStateLaws[A] =
    new CRDTStateLaws[A] { def S: CRDTState[A] = ev }
}
