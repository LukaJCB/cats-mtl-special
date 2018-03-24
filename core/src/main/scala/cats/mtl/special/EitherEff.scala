package cats.mtl.special

import cats.MonadError
import cats.syntax.all._

class EitherEff[F[_], E, A](val absolve: F[A])(implicit F: MonadError[F, Throwable]) {

  def flatMap[B](f: A => EitherEff[F, E, B]): EitherEff[F, E, B] =
    new EitherEff(absolve.flatMap(f andThen (_.absolve)))

  def handleErrorWith(f: E => EitherEff[F, E, A]): EitherEff[F, E, A] =
    new EitherEff(F.handleErrorWith(absolve) {
      case EitherEff.CustomException(e) => f(e.asInstanceOf[E]).absolve
      case e => F.raiseError[A](e)
    })

  def value: F[Either[E, A]] = F.attempt(absolve).map {
    case r @ Right(_) => r.leftCast[E].asRight[Throwable]
    case Left(EitherEff.CustomException(e)) => e.asInstanceOf[E].asLeft[A].asRight[Throwable]
    case l @ Left(_) => l.rightCast[Either[E, A]]
  }.rethrow



}

object EitherEff {
  private[special] case class CustomException[E](e: E) extends Exception

  final def left[B]: LeftPartiallyApplied[B] = new LeftPartiallyApplied[B]

  final def leftT[F[_], B]: LeftTPartiallyApplied[F, B] = new LeftTPartiallyApplied[F, B]

  final def right[F[_], E]: PurePartiallyApplied[F, E] = new PurePartiallyApplied[F, E]

  final def pure[F[_], E]: PurePartiallyApplied[F, E] = right[F, E]

  final def rightT[E]: RightPartiallyApplied[E] = new RightPartiallyApplied[E]

  def raiseError[F[_], A]: LeftTPartiallyApplied[F, A] = leftT[F, A]

  def liftF[F[_], E, A](fa: F[A])(implicit F: MonadError[F, Throwable]): EitherEff[F, E, A] =
    rightT[E](fa)


  implicit def catsMtlSpecialMonadErrorForEitherEff[F[_], E](implicit F: MonadError[F, Throwable]): MonadError[EitherEff[F, E, ?], E] =
    new MonadError[EitherEff[F, E, ?], E] {
      def raiseError[A](e: E): EitherEff[F, E, A] = EitherEff.leftT[F, A](e)

      def pure[A](x: A): EitherEff[F, E, A] = EitherEff.pure[F, E](x)

      def handleErrorWith[A](fa: EitherEff[F, E, A])(f: E => EitherEff[F, E, A]): EitherEff[F, E, A] =
        fa.handleErrorWith(f)

      def flatMap[A, B](fa: EitherEff[F, E, A])(f: A => EitherEff[F, E, B]): EitherEff[F, E, B] =
        fa.flatMap(f)

      def tailRecM[A, B](a: A)(f: A => EitherEff[F, E, Either[A, B]]): EitherEff[F, E, B] =
        new EitherEff(F.tailRecM(a)(a0 => f(a0).absolve))
    }

  private[special] final class LeftPartiallyApplied[B](val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], A](fa: F[A])(implicit F: MonadError[F, Throwable]): EitherEff[F, A, B] =
      new EitherEff(fa.flatMap(a => F.raiseError(CustomException(a))))
  }

  private[special] final class LeftTPartiallyApplied[F[_], B](val dummy: Boolean = true) extends AnyVal {
    def apply[A](a: A)(implicit F: MonadError[F, Throwable]): EitherEff[F, A, B] =
      new EitherEff(F.raiseError(CustomException(a)))
  }

  private[special] final class PurePartiallyApplied[F[_], A](val dummy: Boolean = true) extends AnyVal {
    def apply[B](b: B)(implicit F: MonadError[F, Throwable]): EitherEff[F, A, B] =
      new EitherEff(F.pure(b))
  }

  private[special] final class RightPartiallyApplied[A](val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], B](fb: F[B])(implicit F: MonadError[F, Throwable]): EitherEff[F, A, B] =
      new EitherEff(fb)
  }
}
