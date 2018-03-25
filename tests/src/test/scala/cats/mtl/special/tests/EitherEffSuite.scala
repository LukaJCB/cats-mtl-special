package cats.mtl.special.tests

import cats.Eq
import cats.data.EitherT
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
    eqIO[A].contramap(_.embed)

  implicit def eqEitherTIO[A: Eq, E: Eq]: Eq[EitherT[IO, E, A]] =
    eqIO[Either[E, A]].contramap(_.value)

  implicit def arbitraryEitherEffIO[E: Arbitrary, A: Arbitrary: Cogen]: Arbitrary[EitherEff[IO, E, A]] =
    Arbitrary(Gen.oneOf(
      catsEffectLawsArbitraryForIO[A].arbitrary.map(EitherEff.liftF[IO, E, A]),
      implicitly[Arbitrary[E]].arbitrary.map(e => EitherEff.raiseError[IO, A](e))
    ))

  implicit def arbitraryEitherTIO[E: Arbitrary, A: Arbitrary: Cogen]: Arbitrary[EitherT[IO, E, A]] =
    Arbitrary(Gen.oneOf(
      catsEffectLawsArbitraryForIO[A].arbitrary.map(EitherT.liftF[IO, E, A]),
      implicitly[Arbitrary[E]].arbitrary.map(e => EitherT(IO.pure(e.asLeft[A])))
    ))


  test("toEitherT and back is id") {
    forAll { (ee: EitherEff[IO, String, Int]) =>
      EitherEff.fromEitherT(ee.toEitherT) should === (ee)
    }
  }

  test("fromEitherT and back is id") {
    forAll { (eithert: EitherT[IO, String, Int]) =>
      EitherEff.fromEitherT(eithert).toEitherT should === (eithert)
    }
  }

  test("Operations on EitherT should be isomorphic") {
    forAll { (a: EitherT[IO, String, Int], b: EitherT[IO, String, String], f: String => EitherT[IO, String, Int]) =>
      val et = for {
        x <- a
        y <- b
        z <- f(y)
      } yield z

      val ee = for {
        x <- EitherEff.fromEitherT(a)
        y <- EitherEff.fromEitherT(b)
        z <- EitherEff.fromEitherT(f(y))
      } yield z

      et should === (ee.toEitherT)
    }
  }

  checkAll("MonadError[EitherEff[IO, String, ?], String]",
    MonadErrorTests[EitherEff[IO, String, ?], String].monadError[Int, String, Int])

  checkAll("Effect[EitherEff[IO, String, ?]]",
    EffectTests[EitherEff[IO, String, ?]].effect[Int, String, Int])

}
