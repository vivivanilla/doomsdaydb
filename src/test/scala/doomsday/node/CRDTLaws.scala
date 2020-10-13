package doomsday.node

import doomsday.state.CRDTState
import doomsday.state.CRDTOperation
import doomsday.state.CRDTGetValue
import doomsday.state.CRDTApplyMsg
import cats.implicits._
import org.typelevel.discipline.Laws
import org.scalacheck.Prop.forAll
import arbitraries._
import org.scalacheck.Arbitrary

case class CRDTLaws[StateT, MsgT, OpT, ValT]()(implicit
    S: CRDTState[StateT],
    A: CRDTApplyMsg[StateT, MsgT],
    V: CRDTGetValue[StateT, ValT],
    O: CRDTOperation[StateT, OpT, MsgT],
    arbOp: Arbitrary[OpT]
) extends Laws {

  def operationChangesValues(x: Node[StateT, OpT, MsgT], op: OpT) =
    V.getValues(x.messages(op).foldLeft(x.newState(op).state) { case (state, msg) => A.update(state, msg) }) =!= V
      .getValues(x.state)

  def crdt: RuleSet =
    new RuleSet {
      override def name: String = "CRDTgetValue"
      def bases: Seq[(String, Laws#RuleSet)] = Seq.empty
      def parents = Seq.empty
      def props =
        Seq(
          "operationChangesValues" -> forAll(operationChangesValues _)
        )
    }
}
