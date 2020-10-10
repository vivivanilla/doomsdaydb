package doomsday.lift

import doomsday.state.CRDTApplyMsg

case class CRDTLifted[StateT, MsgT](state: StateT, messages: Seq[MsgT])

trait CRDTLiftable[StateT, MsgT, DatatypeT] {
  def lift(state: StateT): CRDTLifted[StateT, MsgT] with DatatypeT
  def unlift(datatype: CRDTLifted[StateT, MsgT])(implicit applyMsg: CRDTApplyMsg[StateT, MsgT]): StateT =
    datatype.messages.foldLeft(datatype.state)(applyMsg.update)
}

object CRDTLifted {
  def lift[StateT, MsgT](state: StateT): CRDTLifted[StateT, MsgT] = CRDTLifted(state, Seq.empty)
}
