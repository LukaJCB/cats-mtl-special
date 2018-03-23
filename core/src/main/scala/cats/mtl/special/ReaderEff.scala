package cats.mtl.special

import cats.data.Kleisli
import cats.{Applicative, FlatMap, Monad}
import cats.effect.{ConcurrentEffect, Concurrent, Effect, IO, Sync, Async, Fiber}
import cats.mtl.ApplicativeAsk
import cats.syntax.all._

class ReaderEff[F[_]: FlatMap, R, A]private[special](private[special] val value: F[A]) {

  def unsafeRun(r: R)(cb: Either[Throwable, A] => IO[Unit])(implicit F: Effect[F]): Unit =
    F.runAsync(run(r))(cb).unsafeRunSync()

  private[special] def run(r: R)(implicit F: Sync[F]): F[A] =
    F.delay(ReaderEff.cell = r) *> value

  def flatMap[B](f: A => ReaderEff[F, R, B]): ReaderEff[F, R, B] =
    new ReaderEff(value.flatMap(f andThen (_.value)))

  def map[B](f: A => B): ReaderEff[F, R, B] =
    new ReaderEff(value.map(f))

}

private[special] sealed trait ReaderEffFunctions {
  def apply[F[_]: Sync, R, A](f: R => F[A]): ReaderEff[F, R, A] =
    new ReaderEff(ask[F, R].value.flatMap(f))

  def ask[F[_]: Sync, R]: ReaderEff[F, R, R] =
    new ReaderEff(Sync[F].delay(ReaderEff.cell.asInstanceOf[R]))

  def liftF[F[_]: FlatMap, R, A](x: F[A]): ReaderEff[F, R, A] =
    new ReaderEff[F, R, A](x)

  def pure[F[_], R, A](x: A)(implicit F: Monad[F]): ReaderEff[F, R, A] =
    new ReaderEff[F, R, A](F.pure(x))

  def fromKleisli[F[_]: Sync, R, A](k: Kleisli[F, R, A]): ReaderEff[F, R, A] =
    apply(k.run)

  private[special] var cell: Any = _
}

private[special] sealed abstract class ReaderEffInstances extends ReaderEffInstances0 {

  def effFiber[F[_]: FlatMap, R, A](f: Fiber[F, A]): Fiber[ReaderEff[F, R, ?], A] =
    Fiber(new ReaderEff(f.join), new ReaderEff(f.cancel))

  implicit def concurrentEffectForReaderEff[F[_]: ConcurrentEffect, R]: ConcurrentEffect[ReaderEff[F, R, ?]] =
    new ConcurrentEffect[ReaderEff[F, R, ?]] {
      def tailRecM[A, B](a: A)(f: A => ReaderEff[F, R, Either[A, B]]): ReaderEff[F, R, B] =
        new ReaderEff(Monad[F].tailRecM(a)(f andThen (_.value)))

      def flatMap[A, B](fa: ReaderEff[F, R, A])(f: A => ReaderEff[F, R, B]): ReaderEff[F, R, B] =
        fa.flatMap(f)

      def pure[A](x: A): ReaderEff[F, R, A] = new ReaderEff(Monad[F].pure(x))

      def handleErrorWith[A](fa: ReaderEff[F, R, A])(f: Throwable => ReaderEff[F, R, A]): ReaderEff[F, R, A] =
        new ReaderEff[F, R, A](fa.value.handleErrorWith(f andThen (_.value)))

      def raiseError[A](e: Throwable): ReaderEff[F, R, A] =
        new ReaderEff[F, R, A](Sync[F].raiseError(e))

      def async[A](k: (Either[Throwable, A] => Unit) => Unit): ReaderEff[F, R, A] =
        new ReaderEff[F, R, A](Async[F].async(k))

      def cancelable[A](k: (Either[Throwable, A] => Unit) => IO[Unit]): ReaderEff[F, R, A] =
        new ReaderEff[F, R, A](Concurrent[F].cancelable(k))

      def onCancelRaiseError[A](fa: ReaderEff[F, R, A], e: Throwable): ReaderEff[F, R, A] =
        new ReaderEff[F, R, A](Concurrent[F].onCancelRaiseError(fa.value, e))

      def racePair[A, B](fa: ReaderEff[F, R, A], fb: ReaderEff[F, R, B]): ReaderEff[F, R, Either[(A, Fiber[ReaderEff[F, R, ?], B]), (Fiber[ReaderEff[F, R, ?], A], B)]] =
        new ReaderEff(Concurrent[F].racePair(fa.value, fb.value).map { e =>
          e.bimap({
            case (a, f) => (a, effFiber(f))
          }, {
            case (f, b) => (effFiber(f), b)
          })
        })

      def start[A](fa: ReaderEff[F, R, A]): ReaderEff[F, R, Fiber[ReaderEff[F, R, ?], A]] =
        new ReaderEff(Concurrent[F].start(fa.value).map(f => effFiber(f)(ConcurrentEffect[F])))

      def uncancelable[A](fa: ReaderEff[F, R, A]): ReaderEff[F, R, A] =
        new ReaderEff[F, R, A](Concurrent[F].uncancelable(fa.value))

      def runCancelable[A](fa: ReaderEff[F, R, A])(cb: Either[Throwable, A] => IO[Unit]): IO[IO[Unit]] =
        ConcurrentEffect[F].runCancelable(fa.value)(cb)

      def runAsync[A](fa: ReaderEff[F, R, A])(cb: Either[Throwable, A] => IO[Unit]): IO[Unit] =
        Effect[F].runAsync(fa.value)(cb)

      def suspend[A](thunk: => ReaderEff[F, R, A]): ReaderEff[F, R, A] =
        new ReaderEff[F, R, A](Sync[F].suspend(thunk.value))

    }

}

private[special] sealed abstract class ReaderEffInstances0 {

  implicit def monadForReaderEff[F[_]: Monad, R]: Monad[ReaderEff[F, R, ?]] =
    new Monad[ReaderEff[F, R, ?]] {
      def tailRecM[A, B](a: A)(f: A => ReaderEff[F, R, Either[A, B]]): ReaderEff[F, R, B] =
        new ReaderEff(Monad[F].tailRecM(a)(f andThen (_.value)))

      def flatMap[A, B](fa: ReaderEff[F, R, A])(f: A => ReaderEff[F, R, B]): ReaderEff[F, R, B] =
        fa.flatMap(f)

      def pure[A](x: A): ReaderEff[F, R, A] = new ReaderEff(Monad[F].pure(x))
    }

  implicit def applicativeAskForReaderEff[F[_]: Sync, R]: ApplicativeAsk[ReaderEff[F, R, ?], R] =
    new ApplicativeAsk[ReaderEff[F, R, ?], R] {
      val applicative: Applicative[ReaderEff[F, R, ?]] = monadForReaderEff[F, R]

      def ask: ReaderEff[F, R, R] = ReaderEff.ask[F, R]

      def reader[A](f: R => A): ReaderEff[F, R, A] = applicative.map(ask)(f)
    }
}

object ReaderEff extends ReaderEffInstances0 with ReaderEffFunctions {


}
