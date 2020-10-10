package doomsday.state

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import cats.kernel.laws.discipline.SemilatticeTests
import cats.kernel.laws.discipline._
import cats.kernel.Eq
import org.typelevel.discipline.Laws
import doomsday.timestamp.VectorClock

trait CRDTStateTests[A] extends SemilatticeTests[A] {

  def laws: CRDTStateLaws[A]

  def crdtState(implicit arbA: Arbitrary[A], eqA: Eq[A], arbTS: Arbitrary[VectorClock]): RuleSet =
    new RuleSet {
      override def name: String = "CRDTState"
      def bases: Seq[(String, Laws#RuleSet)] = Seq(("Semilattice", semilattice))
      def parents = Seq.empty
      def props =
        Seq(
          "cleanDistributivity" -> forAll(laws.cleanDistributivity _),
          "rollbackDistributivity" -> forAll(laws.rollbackDistributivity _)
        )
    }
}

object CRDTStateTests {
  def apply[A: CRDTState]: CRDTStateTests[A] =
    new CRDTStateTests[A] { def laws: CRDTStateLaws[A] = CRDTStateLaws[A] }
}
