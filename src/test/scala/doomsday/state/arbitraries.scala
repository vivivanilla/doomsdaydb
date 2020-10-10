package doomsday.state

import org.scalacheck.{Arbitrary, Gen}
import cats.kernel.Eq
import doomsday.timestamp.VectorClock
import cats.implicits._
import doomsday.timestamp.arbitraries._

object arbitraries {

  val valueForVectorClock = Arbitrary.arbitrary[VectorClock => Int].sample.get

  implicit def arbLWWRegisterStateInt: Arbitrary[LWWRegisterState[Int]] =
    Arbitrary(
      for {
        rounds <- Gen.choose(0, 20)
        timestamps <- Gen.listOfN(rounds, Arbitrary.arbitrary[VectorClock])
      } yield timestamps.foldLeft(LWWRegisterState[Int](Map.empty)) {
        case (state, timestamp) => state.update(timestamp, valueForVectorClock(timestamp))
      }
    )

  implicit def eqVectorClock: Eq[LWWRegisterState[Int]] = LWWRegisterState.state[Int]

  implicit def arbFun[A](implicit arb: Arbitrary[A]): Arbitrary[A => A] = Arbitrary(arb.arbitrary map ((_: A) => _))
}
