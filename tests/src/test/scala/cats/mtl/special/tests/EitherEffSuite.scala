package cats.mtl.special.tests

import cats.Eq
import cats.effect.IO
import cats.effect.laws.discipline.EffectTests
import cats.tests.CatsSuite
import cats.effect.laws.discipline.arbitrary.catsEffectLawsArbitraryForIO
import cats.effect.laws.util.{TestContext, TestInstances}
import cats.laws.discipline.MonadErrorTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.mtl.special.EitherEff
import org.scalacheck.{Arbitrary, Cogen, Gen}

class EitherEffSuite extends CatsSuite {

  implicit val throwableEq: Eq[Throwable] =
    Eq.by[Throwable, String](_.toString)

  implicit def isoEitherEff[E]: Isomorphisms[EitherEff[IO, E, ?]] =
    Isomorphisms.invariant(EitherEff.catsMtlSpecialMonadErrorForEitherEff)

  implicit val testContext: TestContext = TestContext()

  implicit def eqIO[A: Eq] =
    TestInstances.eqIO[A]

  implicit def eqEitherEffIO[A: Eq, E]: Eq[EitherEff[IO, E, A]] =
    eqIO[A].imap(EitherEff.liftF[IO, E, A])(_.embed)

  implicit def arbitraryEitherEffIO[E: Arbitrary, A: Arbitrary: Cogen]: Arbitrary[EitherEff[IO, E, A]] =
    Arbitrary(Gen.oneOf(
      catsEffectLawsArbitraryForIO[A].arbitrary.map(EitherEff.liftF[IO, E, A]),
      implicitly[Arbitrary[E]].arbitrary.map(e => EitherEff.raiseError[IO, A](e))
    ))

  checkAll("MonadError[EitherEff[IO, String, ?], String]",
    MonadErrorTests[EitherEff[IO, String, ?], String].monadError[Int, String, Int])

  checkAll("Effect[EitherEff[IO, String, ?]]",
    EffectTests[EitherEff[IO, String, ?]].effect[Int, String, Int])

}
