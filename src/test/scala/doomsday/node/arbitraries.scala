package doomsday.node

import org.scalacheck.Arbitrary
import doomsday.state.LWWRegisterOp

object arbitraries {

  implicit def arbLWWRegisterOp[T](implicit arbT: Arbitrary[T]): Arbitrary[LWWRegisterOp[T]] =
    Arbitrary(
      for {
        t <- arbT.arbitrary
      } yield LWWRegisterOp.Set(t)
    )
}
