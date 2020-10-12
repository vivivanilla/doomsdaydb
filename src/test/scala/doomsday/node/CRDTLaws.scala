package doomsday.node

import cats.kernel.Eq
import doomsday.state.CRDTState
import doomsday.state.CRDTOperation
import doomsday.state.CRDTGetValue
import doomsday.state.CRDTApplyMsg
import org.scalacheck.Prop
import cats.implicits._

trait CRDTLaws[StateT, MsgT, OpT, ValT] {
  implicit def eqState: Eq[StateT]
  implicit def eqVal: Eq[ValT]
  implicit def S: CRDTState[StateT]
  implicit def A: CRDTApplyMsg[StateT, MsgT]
  implicit def V: CRDTGetValue[StateT, ValT]
  implicit def O: CRDTOperation[StateT, OpT, MsgT]

  def operationChangesValues(x: Node[StateT, OpT, MsgT], op: OpT): Prop =
    V.getValues(x.messages(op).foldLeft(x.newState(op).state) { case (state, msg) => A.update(state, msg) }) =!= V
      .getValues(x.state)
}

object CRDTLaws {
  def apply[StateT, MsgT, OpT, ValT](implicit
      eqs: Eq[StateT],
      eqv: Eq[ValT],
      s: CRDTState[StateT],
      a: CRDTApplyMsg[StateT, MsgT],
      v: CRDTGetValue[StateT, ValT],
      o: CRDTOperation[StateT, OpT, MsgT]
  ): CRDTLaws[StateT, MsgT, OpT, ValT] =
    new CRDTLaws[StateT, MsgT, OpT, ValT] {
      def eqState = eqs
      def eqVal = eqv
      def S = s
      def A = a
      def V = v
      def O = o
    }
}
