package doomsday.timestamp

import org.scalacheck.{Arbitrary, Gen, Cogen}
import cats.kernel.Eq

object arbitraries {
  implicit def arbVectorClock: Arbitrary[VectorClock] =
    Arbitrary(
      Gen
        .choose(0, 5)
        .flatMap(
          Gen.mapOfN(
            _,
            for {
              nodeId <- Gen.identifier
              time <- Gen.posNum[Long]
            } yield (nodeId, time)
          )
        )
        .map(VectorClock(_))
    )

  // I love you, @larsh
  implicit def coarbVectorClock: Cogen[VectorClock] = implicitly[Cogen[Map[String, Long]]].contramap(_.time)

  implicit def eqVectorClock: Eq[VectorClock] = VectorClock.eq

  implicit def arbFun[A](implicit arb: Arbitrary[A]): Arbitrary[A => A] = Arbitrary(arb.arbitrary map ((_: A) => _))
}
