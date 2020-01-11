package io.github.tgeng.zealot.parsec

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

val empty: Parser[Any, Unit] = pure(()) withName "<empty>"
def any[I] : Parser[I, I] = satisfy[I](_ => true) withName "<any>"
val eof : Parser[Any, Unit] = not(any) withName "eof"
val skip : Parser[Any, Unit] = satisfy[Any](_ => true).map(_ => ()) withName "<skip>"

def [I, T](p: Parser[I, T])* = new Parser[I, IndexedSeq[T]]{
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I], IndexedSeq[T]] = {
    val result = ArrayBuffer[T]()
    breakable {
      val startPosition = input.position
      p.parse(input) match {
        case Right(t) => result += t
        case Left(e) if (startPosition >= input.commitPosition) => {
          input.position = startPosition
          break
        }
        case Left(e) => return Left(e)
      }
    }
    Right(result.toIndexedSeq)
  }
}

def [I, T](p: Parser[I, T])+ : Parser[I, IndexedSeq[T]] = for {
  t <- p
  ts <- p*
} yield t +: ts

def [I, T](p: Parser[I, T])? : Parser[I, Option[T]] = p.map(Some[T]) | empty.map(_ => None)

def [I, T](count: Int) *(p: Parser[I, T]) = new Parser[I, IndexedSeq[T]] {
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I], IndexedSeq[T]] = {
    val position = input.position;
    val result = new ArrayBuffer[T](count)
    for (i <- 0 until count) {
      p.parse(input) match {
        case Right(t) => result += t
        case Left(e) => return Left(ParserError(position, this, e))
      }
    }
    Right(result.toIndexedSeq)
  }
}

def [I, T](p: Parser[I, T]) sepBy1 (s: Parser[I, ?]) : Parser[I, IndexedSeq[T]] = for {
  first <- p
  rest <- (s >> p)*
} yield first +: rest

def [I, T](p: Parser[I, T]) sepBy (s: Parser[I, ?]) : Parser[I, IndexedSeq[T]] = p sepBy1 s | empty

private val prefixSuffixKind : Kind = Kind(4)

def [I, T](p1: Parser[I, ?]) >> (p2: => Parser[I, T]) : Parser[I, T] = new Parser[I, T] {
  override val kind = prefixSuffixKind
  override def parseImpl(input: ParserState[I]) = for {
    r1 <- p1.parse(input)
    r2 <- p2.parse(input)
  } yield r2
}

def [I, T](p1: Parser[I, T]) << (p2: => Parser[I, ?]) : Parser[I, T] = new Parser[I, T] {
  override val kind = prefixSuffixKind
  override def parseImpl(input: ParserState[I]) = for {
    r1 <- p1.parse(input)
    r2 <- p2.parse(input)
  } yield r1
}

def [I, F, T](fnP: Parser[I, F => T]) apply (fP: => Parser[I, F]) : Parser[I, T] = for {
  fn <- fnP
  f <- fP
} yield fn(f)

def [I, F1, F2, T](fnP: Parser[I, (F1, F2) => T]) apply (
  f1P: => Parser[I, F1],
  f2P: => Parser[I, F2],
  ) : Parser[I, T] = for {
  fn <- fnP
  f1 <- f1P
  f2 <- f2P
} yield fn(f1, f2)

def [I, F1, F2, F3, T](fnP: Parser[I, (F1, F2, F3) => T]) apply (
  f1P: => Parser[I, F1],
  f2P: => Parser[I, F2],
  f3P: => Parser[I, F3],
  ) : Parser[I, T] = for {
  fn <- fnP
  f1 <- f1P
  f2 <- f2P
  f3 <- f3P
} yield fn(f1, f2, f3)

def [I, F1, F2, F3, F4, T](fnP: Parser[I, (F1, F2, F3, F4) => T]) apply (
  f1P: => Parser[I, F1],
  f2P: => Parser[I, F2],
  f3P: => Parser[I, F3],
  f4P: => Parser[I, F4],
  ) : Parser[I, T] = for {
  fn <- fnP
  f1 <- f1P
  f2 <- f2P
  f3 <- f3P
  f4 <- f4P
} yield fn(f1, f2, f3, f4)
