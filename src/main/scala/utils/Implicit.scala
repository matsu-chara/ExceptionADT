package utils

object Implicit {
  implicit class ExceptionEither[L <: Throwable, R](val ee: Either[L, R]) {
    def map[L2 <: Throwable, R2](f: R => R2)(implicit L2: L :-> L2): Either[L2, R2] = ee match {
      case Left(e)  => Left(L2.cast(e))
      case Right(v) => Right(f(v))
    }

    def flatMap[L2 <: Throwable, R2](f: R => Either[L2, R2])(implicit L2: L :-> L2): Either[L2, R2] = ee match {
      case Left(e)  => Left(L2.cast(e))
      case Right(v) => f(v)
    }

    def as[L2 <: RuntimeException](implicit L2: L :-> L2): Either[L2, R] = ee match {
      case Left(e)  => Left(L2.cast(e))
      case Right(v) => Right(v)
    }
  }
}