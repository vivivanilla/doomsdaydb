package doomsday.node

import doomsday.timestamp.VectorClock
import doomsday.state.CRDTState
import doomsday.state.CRDTOperation

case class Node[StateT: CRDTState, OpT, MsgT](nodeId: String, clock: VectorClock, state: StateT)(implicit
    ops: CRDTOperation[StateT, OpT, MsgT]
) {
  def newState(op: OpT): Node[StateT, OpT, MsgT] =
    Node(nodeId, clock.inc(nodeId), ops.newState(nodeId, clock.inc(nodeId), state, op))
  def messages(op: OpT): Seq[MsgT] = ops.messages(nodeId, clock.inc(nodeId), state, op)
}

object Node {
  def empty[StateT, OpT, MsgT](nodeId: String)(implicit st: CRDTState[StateT], op: CRDTOperation[StateT, OpT, MsgT]) =
    Node(nodeId, VectorClock.empty, st.empty)
}
