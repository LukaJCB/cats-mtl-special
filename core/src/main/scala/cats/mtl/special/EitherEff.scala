package cats.mtl.special

import cats.{Monad, MonadError}
import cats.effect._
import cats.syntax.all._


private[special] sealed abstract class EitherEffOps[F[_], E, A](val ee: EitherEff[F, E, A]) {
  implicit val F: MonadError[F, Throwable]

  def value: F[Either[E, A]] = F.attempt(embed).map {
    case r @ Right(_) => r.leftCast[E].asRight[Throwable]
    case Left(EitherEff.CustomException(e)) => e.asInstanceOf[E].asLeft[A].asRight[Throwable]
    case l @ Left(_) => l.rightCast[Either[E, A]]
  }.rethrow

  def embed: F[A] = EitherEff.embed(ee)
}

object EitherEff extends NewtypeK2 {

  private[cats] def create[F[_], E, A](s: F[A]): Type[F, E, A] =
    s.asInstanceOf[Type[F, E, A]]


  private[cats] def embed[F[_], E, A](e: Type[F, E, A]): F[A] =
    e.asInstanceOf[F[A]]

  implicit def eitherEffOps[F[_], E, A](e: EitherEff[F, E, A])
                                       (implicit F0: MonadError[F, Throwable]): EitherEffOps[F, E, A] =
    new EitherEffOps[F, E, A](e) { implicit val F: MonadError[F, Throwable] = F0 }

  private[special] case class CustomException[E](e: E) extends Exception

  def handleErrorWith[F[_], E, A](fa: EitherEff[F, E, A])
                                 (f: E => EitherEff[F, E, A])
                                 (implicit F: MonadError[F, Throwable]): EitherEff[F, E, A] =
    create(F.handleErrorWith(embed(fa)) {
      case EitherEff.CustomException(e) => embed(f(e.asInstanceOf[E]))
      case e => F.raiseError[A](e)
    })

  final def left[B]: LeftPartiallyApplied[B] = new LeftPartiallyApplied[B]

  final def leftT[F[_], B]: LeftTPartiallyApplied[F, B] = new LeftTPartiallyApplied[F, B]

  final def right[F[_], E]: PurePartiallyApplied[F, E] = new PurePartiallyApplied[F, E]

  final def pure[F[_], E]: PurePartiallyApplied[F, E] = right[F, E]

  final def rightT[E]: RightPartiallyApplied[E] = new RightPartiallyApplied[E]

  def raiseError[F[_], A]: LeftTPartiallyApplied[F, A] = leftT[F, A]

  def liftF[F[_], E, A](fa: F[A])(implicit F: MonadError[F, Throwable]): EitherEff[F, E, A] =
    rightT[E](fa)

  implicit def catsMtlSpecialConcurrentForEitherEff[F[_]: Effect, E]: Effect[EitherEff[F, E, ?]] =
    new Effect[EitherEff[F, E, ?]] {
      def pure[A](x: A): EitherEff[F, E, A] = EitherEff.pure[F, E](x)

      def handleErrorWith[A](fa: EitherEff[F, E, A])(f: Throwable => EitherEff[F, E, A]): EitherEff[F, E, A] =
        create(embed(fa).handleErrorWith(f andThen embed))

      def raiseError[A](e: Throwable): EitherEff[F, E, A] =
        create(MonadError[F, Throwable].raiseError(e))

      def async[A](k: (Either[Throwable, A] => Unit) => Unit): EitherEff[F, E, A] =
        create(Async[F].async(k))

      def runAsync[A](fa: EitherEff[F, E, A])(cb: Either[Throwable,A] => IO[Unit]): IO[Unit] =
        Effect[F].runAsync(embed(fa))(cb)

      def flatMap[A, B](fa: EitherEff[F, E, A])(f: A => EitherEff[F, E, B]): EitherEff[F, E, B] =
        create(embed(fa).flatMap(f andThen embed))

      def tailRecM[A, B](a: A)(f: A => EitherEff[F, E, Either[A, B]]): EitherEff[F, E, B] =
        create(Monad[F].tailRecM(a)(f andThen embed))

      def suspend[A](thunk: => EitherEff[F, E, A]): EitherEff[F, E, A] =
        create(Sync[F].suspend(embed(thunk)))

    }


  implicit def catsMtlSpecialMonadErrorForEitherEff[F[_], E](implicit F: MonadError[F, Throwable]): MonadError[EitherEff[F, E, ?], E] =
    new MonadError[EitherEff[F, E, ?], E] {
      def raiseError[A](e: E): EitherEff[F, E, A] = EitherEff.leftT[F, A](e)

      def pure[A](x: A): EitherEff[F, E, A] = EitherEff.pure[F, E](x)

      def handleErrorWith[A](fa: EitherEff[F, E, A])(f: E => EitherEff[F, E, A]): EitherEff[F, E, A] =
        EitherEff.handleErrorWith[F, E, A](fa)(f)

      def flatMap[A, B](fa: EitherEff[F, E, A])(f: A => EitherEff[F, E, B]): EitherEff[F, E, B] =
        create(embed(fa).flatMap(f andThen embed))

      def tailRecM[A, B](a: A)(f: A => EitherEff[F, E, Either[A, B]]): EitherEff[F, E, B] =
        create(F.tailRecM(a)(f andThen embed))
    }

  private[special] final class LeftPartiallyApplied[B](val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], A](fa: F[A])(implicit F: MonadError[F, Throwable]): EitherEff[F, A, B] =
      create(fa.flatMap(a => F.raiseError(CustomException(a))))
  }

  private[special] final class LeftTPartiallyApplied[F[_], B](val dummy: Boolean = true) extends AnyVal {
    def apply[A](a: A)(implicit F: MonadError[F, Throwable]): EitherEff[F, A, B] =
      create(F.raiseError(CustomException(a)))
  }

  private[special] final class PurePartiallyApplied[F[_], A](val dummy: Boolean = true) extends AnyVal {
    def apply[B](b: B)(implicit F: MonadError[F, Throwable]): EitherEff[F, A, B] =
      create(F.pure(b))
  }

  private[special] final class RightPartiallyApplied[A](val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], B](fb: F[B])(implicit F: MonadError[F, Throwable]): EitherEff[F, A, B] =
      create(fb)
  }
}
