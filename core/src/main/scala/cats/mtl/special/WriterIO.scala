package cats.mtl.special



class WriterIO[F[_], L, A](val value: F[(L, A)]) {

}


object WriterIO {

}
