package doomsday.state

import doomsday.timestamp.VectorClock
import cats.implicits._
import org.scalacheck.Prop._
import cats.kernel.Eq
import org.typelevel.discipline.Laws
import org.scalacheck.Arbitrary
import doomsday.timestamp.arbitraries._

case class CRDTGetValueLaws[StateT, ValT]()(implicit
    S: CRDTState[StateT],
    V: CRDTGetValue[StateT, ValT],
    eqB: Eq[ValT],
    arbState: Arbitrary[StateT]
) extends Laws {

  private def cleanPreservesValue(x: StateT, t: VectorClock) =
    t < V.getUniqueTime(x) ==> (V.getUniqueValue(x) === V.getUniqueValue(S.clean(x, t)))

  private def cleanPreservesValues(x: StateT, t: VectorClock) =
    V.getTimes(x).forall(t < _) ==> (V.getValues(x) === V.getValues(S.clean(x, t)))

  def crdtGetValue: RuleSet =
    new RuleSet {
      override def name: String = "CRDTgetValue"
      def bases: Seq[(String, Laws#RuleSet)] = Seq.empty
      def parents = Seq.empty
      def props =
        Seq(
          "cleanPreservesValue" -> forAll(cleanPreservesValue _),
          "cleanPreservesValues" -> forAll(cleanPreservesValues _)
        )
    }
}
