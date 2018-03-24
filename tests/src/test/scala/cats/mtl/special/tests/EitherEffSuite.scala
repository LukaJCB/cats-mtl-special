package cats.mtl.special.tests

import cats.Eq
import cats.effect.IO
import cats.tests.CatsSuite
import cats.effect.laws.discipline.arbitrary.catsEffectLawsArbitraryForIO
import cats.effect.laws.util.{TestContext, TestInstances}
import cats.laws.discipline.MonadErrorTests
import cats.mtl.special.EitherEff
import org.scalacheck.{Arbitrary, Cogen, Gen}

class EitherEffSuite extends CatsSuite {

  implicit val testContext: TestContext = TestContext()

  implicit def eqEitherEffIO[A: Eq, E]: Eq[EitherEff[IO, E, A]] =
    TestInstances.eqIO[A].imap(EitherEff.liftF[IO, E, A])(_.absolve)

  implicit def arbitraryEitherEffIO[E: Arbitrary, A: Arbitrary: Cogen]: Arbitrary[EitherEff[IO, E, A]] =
    Arbitrary(Gen.oneOf(
      catsEffectLawsArbitraryForIO[A].arbitrary.map(EitherEff.liftF[IO, E, A]),
      implicitly[Arbitrary[E]].arbitrary.map(e => EitherEff.raiseError[IO, A](e))
    ))

  checkAll("EitherEff[IO, String, ?]", MonadErrorTests[EitherEff[IO, String, ?], String].monadError[Int, String, Int])

}
