# cats-mtl-special

This library defines specializations for certain monad transformers to make them more performant.
Currently monad transformers in Scala are extremely slow compared to their Haskell counterparts.
Part of this performance penalty is the fact that monad transformers are extremely general and allow us to stack any amounts of transformers on top of each other.
The idea in this library is to use specialited transformers that only work with very specific underlying monads to improve performance.
These new "transformers" are still polymorphic on their `F` type, but only work when the given `F` defines instances for the various `cats-effect` type classes.
E.g. `ReaderEff[F, R, A]` will only have an instance of `ApplicativeAsk` if `F` has an instance of `cats.effect.Sync`.

## Transformers

Right now the only transformers are `ReaderEff` and `EitherEff`, both are more specific versions of `ReaderT` (`Kleisli`) and `EitherT` respectively.
Future versions should include transformers like `StateEff`, `WriterEff` as well as combinations such as `ReaderEitherEff` or `EitherStateEff`.

## Benchmarks
