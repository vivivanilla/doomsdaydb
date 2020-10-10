package doomsday.state

import doomsday.timestamp.VectorClock
import cats.implicits._
import org.scalacheck.Prop._
import cats.kernel.Eq
import org.scalacheck.Prop

trait CRDTGetValueLaws[A, B] {
  implicit def eqB: Eq[B]
  implicit def S: CRDTState[A]
  implicit def V: CRDTGetValue[A, B]

  def cleanPreservesValue(x: A, t: VectorClock): Prop =
    t < V.getUniqueTime(x) ==> (V.getUniqueValue(x) === V.getUniqueValue(S.clean(x, t)))
}

object CRDTGetValueLaws {
  def apply[A, B](implicit s: CRDTState[A], v: CRDTGetValue[A, B], eqb: Eq[B]): CRDTGetValueLaws[A, B] =
    new CRDTGetValueLaws[A, B] {
      def S = s
      def V = v
      def eqB = eqb
    }
}
