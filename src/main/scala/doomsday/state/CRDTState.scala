package doomsday.state

import cats.kernel.Semilattice
import doomsday.timestamp.VectorClock
import cats.kernel.Eq

trait CRDTState[StateT] extends Semilattice[StateT] with Eq[StateT] {
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
