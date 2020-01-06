package io.github.tgeng.zealot.common

import scala.language.implicitConversions

object EitherSugar {
  given liftL : [L, R] Conversion[L, Either[L, R]] = l => Left(l)
  given liftR : [L, R] Conversion[R, Either[L, R]] = r => Right(r)
}

object OptionSugar {
  def [T](o: Option[T]) orThrow(e : => Exception) : T = o match {
    case Some(t) => t
    case None => throw e
  }
}
