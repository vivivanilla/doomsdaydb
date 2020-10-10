package doomsday.state

import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import org.scalatestplus.scalacheck.Checkers
import cats.implicits._

class LWWRegisterStateTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {
  import doomsday.timestamp.arbitraries._
  import arbitraries._

  implicit override val generatorDrivenConfig =
    // If test LWWRegisterState.CRDTGetValueLaws fails, increase this
    PropertyCheckConfiguration(maxDiscardedFactor = 10)

  checkAll("LWWRegisterState.CRDTStateLaws", CRDTStateTests[LWWRegisterState[Int]].crdtState)

  checkAll(
    "LWWRegisterState.CRDTGetValueLaws",
    CRDTGetValueTests[LWWRegisterState[Int], Int].crdtGetValue
  )
}