package cats.mtl.special

private[special] trait NewtypeK2 { self =>
  private[special] type Base
  private[special] trait Tag extends Any
  type Type[F[_], E, A] <: Base with Tag
}
