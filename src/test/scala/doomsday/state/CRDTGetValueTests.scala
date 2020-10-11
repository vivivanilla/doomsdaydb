package doomsday.state

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import cats.kernel.Eq
import org.typelevel.discipline.Laws
import doomsday.timestamp.arbitraries._

trait CRDTGetValueTests[A, B] extends Laws {

  def laws: CRDTGetValueLaws[A, B]

  def crdtGetValue(implicit arbA: Arbitrary[A]): RuleSet =
    new RuleSet {
      override def name: String = "CRDTgetValue"
      def bases: Seq[(String, Laws#RuleSet)] = Seq.empty
      def parents = Seq.empty
      def props =
        Seq(
          "cleanPreservesValue" -> forAll(laws.cleanPreservesValue _),
          "cleanPreservesValues" -> forAll(laws.cleanPreservesValues _)
        )
    }
}

object CRDTGetValueTests {
  def apply[A, B](implicit s: CRDTState[A], v: CRDTGetValue[A, B], eqb: Eq[B]): CRDTGetValueTests[A, B] =
    new CRDTGetValueTests[A, B] { def laws: CRDTGetValueLaws[A, B] = CRDTGetValueLaws[A, B] }
}
