package io.github.tgeng.zealot.parsec

import scala.collection.mutable.ArrayBuffer

val empty: Parser[Any, Unit] = pure(()) withName "<empty>"
def any[I] : Parser[I, I] = satisfy[I](_ => true) withName "<any>"
val eof : Parser[Any, Unit] = not(any) withName "<eof>"
val skip : Parser[Any, Unit] = satisfy[Any](_ => true).map(_ => ()) withName "<skip>"
def anyOf[I](candidate : Seq[I]) : Parser[I, I] = satisfy(candidate.contains(_))

def suffixKind = Kind(9, "suffix")

def [I, T](p: Parser[I, T])* = new Parser[I, IndexedSeq[T]](suffixKind){
  override def detailImpl = p.name(kind) + "*"
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I], IndexedSeq[T]] = {
    val result = ArrayBuffer[T]()
    while(true) {
      val startPosition = input.position
      p.parse(input) match {
        case Right(t) => result += t
        case Left(e) if (startPosition >= input.commitPosition) => {
          input.position = startPosition
          return Right(result.toIndexedSeq)
        }
        case Left(e) => return Left(e)
      }
    }
    throw AssertionError("impossible branch")
  }
}

def [I, T](p: Parser[I, T])+ : Parser[I, IndexedSeq[T]] = (for {
  t <- p
  ts <- p*
} yield t +: ts) withDetailAndKind(p.name(suffixKind) + "+", suffixKind)

def [I, T](p: Parser[I, T])? : Parser[I, Option[T]] =
  (p.map(Some[T]) | empty.map(_ => None)) withDetailAndKind(
    p.name(suffixKind) + "?",
    suffixKind
  )

private val repeatKind = Kind(8, "n*_")

def [I, T](count: Int) *(p: Parser[I, T]) = new Parser[I, IndexedSeq[T]](repeatKind) {
  override def detailImpl = s"$count * " + p.name(kind)
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I], IndexedSeq[T]] = {
    import scala.util.control.NonLocalReturns._
    val position = input.position;
    val result = new ArrayBuffer[T](count)
    for (i <- 0 until count) {
      p.parse(input) match {
        case Right(t) => result += t
        case Left(e) => returning { Left(ParserError(position, this, e)) }
      }
    }
    Right(result.toIndexedSeq)
  }
}

val sepKind = Kind(3, "sep")

def [I, T](p: Parser[I, T]) sepBy1 (s: Parser[I, ?]) : Parser[I, IndexedSeq[T]] = (for {
  first <- p
  rest <- (s >> p)*
} yield first +: rest) withDetailAndKind (
  s"${p.name(sepKind)} sepBy1 ${s.name(sepKind)}",
  sepKind
)

def [I, T](p: Parser[I, T]) sepBy (s: Parser[I, ?]) : Parser[I, IndexedSeq[T]] =
  (p sepBy1 s | empty) withDetailAndKind (
  s"${p.name(sepKind)} sepBy ${s.name(sepKind)}",
  sepKind
)

val prefixSuffixKind = Kind(4, "prefixSuffix")

def [I, T](p1: Parser[I, ?]) >> (p2: => Parser[I, T]) : Parser[I, T] = (for {
  _ <- p1
  t <- p2
} yield t) withDetailAndKind (
  p1.name(prefixSuffixKind) + " >> " + p2.name(prefixSuffixKind),
  prefixSuffixKind
)

def [I, T](p1: Parser[I, T]) << (p2: => Parser[I, ?]) : Parser[I, T] = (for {
  t <- p1
  _ <- p2
} yield t) withDetailAndKind (
  p1.name(prefixSuffixKind) + " << " + p2.name(prefixSuffixKind),
  prefixSuffixKind
)

def applyKind = Kind(10, "apply")

def [I, F, T](fnP: Parser[I, F => T]) apply(
  fP: => Parser[I, F]
  ) : Parser[I, T] = (for {
  fn <- fnP
  f <- fP
} yield fn(f)) withDetailAndKind(
  s"${fnP.name(applyKind)}(${fP.name()})",
  applyKind
)

def [I, F1, F2, T](fnP: Parser[I, (F1, F2) => T]) apply(
  f1P: => Parser[I, F1],
  f2P: => Parser[I, F2],
  ) : Parser[I, T] = (for {
  fn <- fnP
  f1 <- f1P
  f2 <- f2P
} yield fn(f1, f2)) withDetailAndKind (
  s"${fnP.name(applyKind)}(${f1P.name()}, ${f2P.name()})",
  applyKind
)

def [I, F1, F2, F3, T](fnP: Parser[I, (F1, F2, F3) => T]) apply(
  f1P: => Parser[I, F1],
  f2P: => Parser[I, F2],
  f3P: => Parser[I, F3],
  ) : Parser[I, T] = (for {
  fn <- fnP
  f1 <- f1P
  f2 <- f2P
  f3 <- f3P
} yield fn(f1, f2, f3)) withDetailAndKind (
  s"${fnP.name(applyKind)}(${f1P.name()}, ${f2P.name()}, ${f3P.name()})",
  applyKind
)

def [I, F1, F2, F3, F4, T](fnP: Parser[I, (F1, F2, F3, F4) => T]) apply(
  f1P: => Parser[I, F1],
  f2P: => Parser[I, F2],
  f3P: => Parser[I, F3],
  f4P: => Parser[I, F4],
  ) : Parser[I, T] = (for {
  fn <- fnP
  f1 <- f1P
  f2 <- f2P
  f3 <- f3P
  f4 <- f4P
} yield fn(f1, f2, f3, f4)) withDetailAndKind (
  s"${fnP.name(applyKind)}(${f1P.name()}, ${f2P.name()}, ${f3P.name()}, ${f4P.name()})",
  applyKind
)
