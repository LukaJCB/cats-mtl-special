package cats.mtl.special
package tests

import cats.Eq
import cats.tests.CatsSuite
import cats.mtl.laws.discipline.ApplicativeAskTests
import cats.effect._
import cats.effect.laws.util.{TestContext, TestInstances}
import cats.effect.laws.discipline.arbitrary.catsEffectLawsArbitraryForIO
import org.scalacheck.{Arbitrary, Cogen, Gen}

class ReaderEffSuite extends CatsSuite {

  implicit val testContext: TestContext = TestContext()

  implicit def eqReaderEffIO[A: Eq]: Eq[ReaderEff[IO, Int, A]] =
    TestInstances.eqIO[A].imap(ReaderEff.liftF[IO, Int, A])(re => ReaderEff.run(re)(123))

  implicit def arbitraryReaderEffIO[R, A: Arbitrary: Cogen]: Arbitrary[ReaderEff[IO, R, A]] =
    Arbitrary(Gen.oneOf(
      catsEffectLawsArbitraryForIO[A].arbitrary.map(ReaderEff.liftF[IO, R, A]),
      implicitly[Arbitrary[A]].arbitrary.map(a => ReaderEff.ask[IO, R].map(_ => a))
    ))

  checkAll("ReaderEff[IO, String, ?]",
    ApplicativeAskTests[ReaderEff[IO, Int, ?], Int]
      .applicativeAsk[Int])
}
