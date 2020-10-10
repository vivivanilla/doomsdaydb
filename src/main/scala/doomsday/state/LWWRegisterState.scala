package doomsday.state

import doomsday.timestamp.VectorClock
import doomsday.message.LWWRegisterMessage
import doomsday.message.Update
import cats.implicits._
import cats.kernel.Eq
import alleycats.Empty

case class LWWRegisterState[T](values: Map[VectorClock, T]) {
  def process(msg: LWWRegisterMessage[T]): LWWRegisterState[T] =
    msg match {
      case Update(time, value) => update(time, value)
    }

  def update(time: VectorClock, value: T) = LWWRegisterState(values + (time -> value))

  def merge(that: LWWRegisterState[T]) = LWWRegisterState(values ++ that.values)

  def getValue = values.get(values.keySet.max(VectorClock.lexicalOrderNodePriorityOrder.toOrdering))
}

object LWWRegisterState {
  implicit def state[T: Eq]: CRDTState[LWWRegisterState[T]] =
    new CRDTState[LWWRegisterState[T]] {

      override def combine(x: LWWRegisterState[T], y: LWWRegisterState[T]): LWWRegisterState[T] = x merge y

      override def clean(state: LWWRegisterState[T], acknowledgedTime: VectorClock): LWWRegisterState[T] =
        LWWRegisterState(state.values.filterNot { case (clock, _) => clock <= acknowledgedTime })

      override def rollback(state: LWWRegisterState[T], undoTime: VectorClock): LWWRegisterState[T] =
        LWWRegisterState(state.values.filterNot { case (clock, _) => clock == undoTime })

      override def eqv(x: LWWRegisterState[T], y: LWWRegisterState[T]): Boolean = x.values === y.values
    }

  implicit def applyMsg[T]: CRDTApplyMsg[LWWRegisterState[T], LWWRegisterMessage[T]] =
    new CRDTApplyMsg[LWWRegisterState[T], LWWRegisterMessage[T]] {

      override def update(state: LWWRegisterState[T], msg: LWWRegisterMessage[T]): LWWRegisterState[T] =
        msg match {
          case Update(time, value) => state.update(time, value)
        }

      override def conflicts(state: LWWRegisterState[T], msg: LWWRegisterMessage[T]): Set[VectorClock] =
        msg match {
          case Update(time, _) => state.values.keySet.filterNot(clk => clk < time || clk > time)
        }
    }

  implicit def value[T: Empty]: CRDTGetValue[LWWRegisterState[T], T] =
    new CRDTGetValue[LWWRegisterState[T], T] {

      override def getUniqueValue(state: LWWRegisterState[T]): T =
        if (state.values.isEmpty) implicitly[Empty[T]].empty
        else state.values.get(state.values.keySet.max(VectorClock.lexicalOrderNodePriorityOrder.toOrdering)).get

      override def getValues(state: LWWRegisterState[T]): Set[T] =
        state.values
          .filter {
            case (clk, _) => !state.values.keySet.exists(_ > clk)
          }
          .values
          .toSet

      override def getUniqueTime(state: LWWRegisterState[T]): VectorClock =
        if (state.values.isEmpty) VectorClock.empty
        else state.values.keySet.max(VectorClock.lexicalOrderNodePriorityOrder.toOrdering)

      override def getTimes(state: LWWRegisterState[T]): Set[VectorClock] =
        state.values.filter {
          case (timestamp, _) => !state.values.exists { case (otherTS, _) => otherTS > timestamp }
        }.keySet

    }
}
