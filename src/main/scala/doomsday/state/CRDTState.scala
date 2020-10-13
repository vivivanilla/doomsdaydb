package doomsday.state

import cats.kernel.Semilattice
import doomsday.timestamp.VectorClock
import cats.kernel.Eq

/**
  * Represents a CRDT state at some node
  */
trait CRDTState[StateT] extends Semilattice[StateT] with Eq[StateT] {
  def empty: StateT
  def clean(state: StateT, acknowledgedTime: VectorClock): StateT
  def rollback(state: StateT, undoTime: VectorClock): StateT
}

trait CRDTApplyMsg[StateT, MsgT] {
  def update(state: StateT, msg: MsgT): StateT
  def conflicts(state: StateT, msg: MsgT): Set[VectorClock]
}

trait CRDTGetValue[StateT, ValT] {
  def getUniqueValue(state: StateT): ValT
  def getValues(state: StateT): Set[ValT]
  def getUniqueTime(state: StateT): VectorClock
  def getTimes(state: StateT): Set[VectorClock]
}

trait CRDTOperation[StateT, OpT, MsgT] {
  def messages(nodeId: String, nodeClock: Long, state: StateT, op: OpT): Seq[MsgT]
  def newState(nodeId: String, nodeClock: Long, state: StateT, op: OpT): StateT
}
