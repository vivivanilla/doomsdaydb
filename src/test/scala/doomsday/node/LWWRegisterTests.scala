package doomsday.node

import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import org.scalatestplus.scalacheck.Checkers
import cats.implicits._
import doomsday.state.LWWRegisterState
import doomsday.message.LWWRegisterMessage
import doomsday.state.LWWRegisterOp
import arbitraries._

class LWWRegisterTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {

  checkAll(
    "LWWRegisterState",
    IndependentNodesTest[LWWRegisterState[Int], LWWRegisterMessage[Int], LWWRegisterOp[Int], Int].crdt
  )

  checkAll(
    "LWWRegister",
    ConcurrentNodesTest[LWWRegisterState[Int], LWWRegisterMessage[Int], LWWRegisterOp[Int], Int].crdt
  )
}
