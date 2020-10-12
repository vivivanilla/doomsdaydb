package doomsday.state

sealed trait LWWRegisterOp[T]

object LWWRegisterOp {
  case class Set[T](newValue: T) extends LWWRegisterOp[T]
}
