package doomsday.message

import doomsday.timestamp.VectorClock

sealed trait LWWRegisterMessage[T]

case class Update[T](time: VectorClock, value: T) extends LWWRegisterMessage[T]
