package doomsday.state

import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import org.scalatestplus.scalacheck.Checkers
import cats.implicits._

class LWWRegisterStateTests extends AnyFunSuite with FunSuiteDiscipline with Checkers {
  import arbitraries._

  implicit override val generatorDrivenConfig =
    // If test LWWRegisterState.CRDTGetValueLaws fails, increase this
    PropertyCheckConfiguration(maxDiscardedFactor = 20)

  checkAll("LWWRegisterState.CRDTStateLaws", CRDTStateLaws[LWWRegisterState[Int]].crdtState)

  checkAll("LWWRegisterState.CRDTGetValueLaws", CRDTGetValueLaws[LWWRegisterState[Int], Int].crdtGetValue)
}
