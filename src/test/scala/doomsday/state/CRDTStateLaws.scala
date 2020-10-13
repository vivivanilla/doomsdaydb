package doomsday.state

import doomsday.timestamp.VectorClock
import org.scalacheck.Arbitrary
import cats.implicits._
import org.typelevel.discipline.Laws
import org.scalacheck.Prop.forAll
import doomsday.timestamp.arbitraries._
import cats.kernel.laws.discipline.SemilatticeTests
import cats.kernel.laws.discipline.EqTests

case class CRDTStateLaws[A]()(implicit
    S: CRDTState[A],
    A: Arbitrary[A]
) extends Laws {

  private def cleanDistributivity(x: A, y: A, t: VectorClock) =
    S.combine(S.clean(x, t), S.clean(y, t)) === S.clean(S.combine(x, y), t)

  private def rollbackDistributivity(x: A, y: A, t: VectorClock) =
    S.combine(S.rollback(x, t), S.rollback(y, t)) === S.rollback(S.combine(x, y), t)

  def crdtState: RuleSet =
    new RuleSet {
      override def name: String = "CRDTState"
      def bases: Seq[(String, Laws#RuleSet)] =
        Seq(
          "Semilattice" -> SemilatticeTests[A].semilattice,
          "Eq" -> EqTests[A].eqv
        )
      def parents = Seq.empty
      def props =
        Seq(
          "cleanDistributivity" -> forAll(cleanDistributivity _),
          "rollbackDistributivity" -> forAll(rollbackDistributivity _)
        )
    }

}
