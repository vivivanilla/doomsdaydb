package doomsday.node

import doomsday.state.CRDTState
import doomsday.state.CRDTOperation
import doomsday.state.CRDTApplyMsg

case class Node[StateT: CRDTState, OpT, MsgT](nodeId: String, nodeClock: Long, state: StateT)(implicit
    ops: CRDTOperation[StateT, OpT, MsgT]
) {
  def newState(op: OpT): Node[StateT, OpT, MsgT] =
    Node(nodeId, nodeClock + 1, ops.newState(nodeId, nodeClock + 1, state, op))
  def messages(op: OpT): Seq[MsgT] = ops.messages(nodeId, nodeClock + 1, state, op)

  def processMsg(msg: MsgT)(implicit a: CRDTApplyMsg[StateT, MsgT]): Node[StateT, OpT, MsgT] =
    Node(nodeId, nodeClock + 1, a.update(state, msg))
}

object Node {
  def empty[StateT, OpT, MsgT](nodeId: String)(implicit st: CRDTState[StateT], op: CRDTOperation[StateT, OpT, MsgT]) =
    Node(nodeId, 1, st.empty)
}
