package cats.mtl.special

import cats.data.Kleisli
import cats.{Applicative, FlatMap, Monad}
import cats.effect.{Async, Concurrent, ConcurrentEffect, Effect, Fiber, IO, Sync}
import cats.mtl.ApplicativeAsk
import cats.syntax.all._

private[special] sealed abstract class ReaderEffOps[F[_], R, A](val re: ReaderEff[F, R, A]) {
  def unsafeRun(r: R)(cb: Either[Throwable, A] => IO[Unit])(implicit F: Effect[F]): Unit =
    F.runAsync(ReaderEff.run(re)(r))(cb).unsafeRunSync()

}

private[special] sealed trait ReaderEffFunctions {

  private[special] def run[F[_], R, A](re: ReaderEff[F, R, A])(r: R)(implicit F: Sync[F]): F[A] =
    F.delay(ReaderEff.cell = r) *> ReaderEff.unwrap(re)

  def apply[F[_]: Sync, R, A](f: R => F[A]): ReaderEff[F, R, A] =
    ReaderEff.create( ReaderEff.unwrap(ask[F, R]).flatMap(f))

  def ask[F[_]: Sync, R]: ReaderEff[F, R, R] =
    ReaderEff.create(Sync[F].delay(ReaderEff.cell.asInstanceOf[R]))

  def liftF[F[_]: FlatMap, R, A](x: F[A]): ReaderEff[F, R, A] =
    ReaderEff.create[F, R, A](x)

  def pure[F[_], R, A](x: A)(implicit F: Monad[F]): ReaderEff[F, R, A] =
    ReaderEff.create[F, R, A](F.pure(x))

  def fromKleisli[F[_]: Sync, R, A](k: Kleisli[F, R, A]): ReaderEff[F, R, A] =
    apply(k.run)

  def flatMap[F[_]: FlatMap, R, A, B](fa: ReaderEff[F, R, A])(f: A => ReaderEff[F, R, B]): ReaderEff[F, R, B] =
    ReaderEff.create(ReaderEff.unwrap(fa).flatMap(f andThen  ReaderEff.unwrap))

  private[special] var cell: Any = _
}

private[special] sealed abstract class ReaderEffInstances extends ReaderEffInstances0 {

  def effFiber[F[_]: FlatMap, R, A](f: Fiber[F, A]): Fiber[ReaderEff[F, R, ?], A] =
    Fiber(ReaderEff.create(f.join), ReaderEff.create(f.cancel))

  implicit def concurrentEffectForReaderEff[F[_]: ConcurrentEffect, R]: ConcurrentEffect[ReaderEff[F, R, ?]] =
    new ConcurrentEffect[ReaderEff[F, R, ?]] {
      def tailRecM[A, B](a: A)(f: A => ReaderEff[F, R, Either[A, B]]): ReaderEff[F, R, B] =
        ReaderEff.create(Monad[F].tailRecM(a)(f andThen ReaderEff.unwrap))

      def flatMap[A, B](fa: ReaderEff[F, R, A])(f: A => ReaderEff[F, R, B]): ReaderEff[F, R, B] =
        ReaderEff.flatMap(fa)(f)

      override def map[A, B](fa: ReaderEff[F, R, A])(f: A => B): ReaderEff[F, R, B] =
        ReaderEff.create(ReaderEff.unwrap(fa).map(f))

      def pure[A](x: A): ReaderEff[F, R, A] = ReaderEff.create(Monad[F].pure(x))

      def handleErrorWith[A](fa: ReaderEff[F, R, A])(f: Throwable => ReaderEff[F, R, A]): ReaderEff[F, R, A] =
        ReaderEff.create[F, R, A](ReaderEff.unwrap(fa).handleErrorWith(f andThen ReaderEff.unwrap))

      def raiseError[A](e: Throwable): ReaderEff[F, R, A] =
        ReaderEff.create[F, R, A](Sync[F].raiseError(e))

      def async[A](k: (Either[Throwable, A] => Unit) => Unit): ReaderEff[F, R, A] =
        ReaderEff.create[F, R, A](Async[F].async(k))

      def cancelable[A](k: (Either[Throwable, A] => Unit) => IO[Unit]): ReaderEff[F, R, A] =
        ReaderEff.create[F, R, A](Concurrent[F].cancelable(k))

      def onCancelRaiseError[A](fa: ReaderEff[F, R, A], e: Throwable): ReaderEff[F, R, A] =
        ReaderEff.create[F, R, A](Concurrent[F].onCancelRaiseError(ReaderEff.unwrap(fa), e))

      def racePair[A, B](fa: ReaderEff[F, R, A], fb: ReaderEff[F, R, B]): ReaderEff[F, R, Either[(A, Fiber[ReaderEff[F, R, ?], B]), (Fiber[ReaderEff[F, R, ?], A], B)]] =
        ReaderEff.create(Concurrent[F].racePair(ReaderEff.unwrap(fa), ReaderEff.unwrap(fb)).map { e =>
          e.bimap({
            case (a, f) => (a, effFiber(f))
          }, {
            case (f, b) => (effFiber(f), b)
          })
        })

      def start[A](fa: ReaderEff[F, R, A]): ReaderEff[F, R, Fiber[ReaderEff[F, R, ?], A]] =
        ReaderEff.create(Concurrent[F].start(ReaderEff.unwrap(fa)).map(f => effFiber(f)(ConcurrentEffect[F])))

      def uncancelable[A](fa: ReaderEff[F, R, A]): ReaderEff[F, R, A] =
        ReaderEff.create[F, R, A](Concurrent[F].uncancelable(ReaderEff.unwrap(fa)))

      def runCancelable[A](fa: ReaderEff[F, R, A])(cb: Either[Throwable, A] => IO[Unit]): IO[IO[Unit]] =
        ConcurrentEffect[F].runCancelable(ReaderEff.unwrap(fa))(cb)

      def runAsync[A](fa: ReaderEff[F, R, A])(cb: Either[Throwable, A] => IO[Unit]): IO[Unit] =
        Effect[F].runAsync(ReaderEff.unwrap(fa))(cb)

      def suspend[A](thunk: => ReaderEff[F, R, A]): ReaderEff[F, R, A] =
        ReaderEff.create[F, R, A](Sync[F].suspend(ReaderEff.unwrap(thunk)))

    }

}

private[special] sealed abstract class ReaderEffInstances0 {

  implicit def monadForReaderEff[F[_]: Monad, R]: Monad[ReaderEff[F, R, ?]] =
    new Monad[ReaderEff[F, R, ?]] {
      def tailRecM[A, B](a: A)(f: A => ReaderEff[F, R, Either[A, B]]): ReaderEff[F, R, B] =
        ReaderEff.create(Monad[F].tailRecM(a)(f andThen ReaderEff.unwrap))

      def flatMap[A, B](fa: ReaderEff[F, R, A])(f: A => ReaderEff[F, R, B]): ReaderEff[F, R, B] =
        ReaderEff.flatMap(fa)(f)

      def pure[A](x: A): ReaderEff[F, R, A] = ReaderEff.create(Monad[F].pure(x))
    }

  implicit def applicativeAskForReaderEff[F[_]: Sync, R]: ApplicativeAsk[ReaderEff[F, R, ?], R] =
    new ApplicativeAsk[ReaderEff[F, R, ?], R] {
      val applicative: Applicative[ReaderEff[F, R, ?]] = monadForReaderEff[F, R]

      def ask: ReaderEff[F, R, R] = ReaderEff.ask[F, R]

      def reader[A](f: R => A): ReaderEff[F, R, A] = applicative.map(ask)(f)
    }
}

object ReaderEff extends ReaderEffInstances with ReaderEffFunctions with NewtypeK2 {

  private[cats] def create[F[_], E, A](s: F[A]): Type[F, E, A] =
    s.asInstanceOf[Type[F, E, A]]

  private[cats] def unwrap[F[_], E, A](e: Type[F, E, A]): F[A] =
    e.asInstanceOf[F[A]]

  implicit def readerEffOps[F[_], R, A](re: ReaderEff[F, R, A]): ReaderEffOps[F, R, A] =
    new ReaderEffOps[F, R, A](re) {}

}
