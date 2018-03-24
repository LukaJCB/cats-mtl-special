package cats.mtl

package object special {
  type EitherEff[F[_], E, A] = EitherEff.Type[F, E, A]

  type ReaderEff[F[_], R, A] = ReaderEff.Type[F, R, A]
}
