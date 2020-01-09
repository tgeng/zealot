package io.github.tgeng.zealot.parsec

import scala.util.control.Breaks._

def [I, T](p: => Parser[I, T])* : Parser[I, IndexedSeq[T]] = new Parser[I, IndexedSeq[T]]{
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I], IndexedSeq[T]] = {
    val result = scala.collection.mutable.ArrayBuffer[T]()
    breakable {
      val startPosition = input.position
      p.parse(input) match {
        case Right(t) => result += t
        case Left(e) => {
          input.position = startPosition
          break
        }
      }
    }
    Right(result.toIndexedSeq)
  }
}

def [I, T](p: => Parser[I, T])+ : Parser[I, IndexedSeq[T]] = for {
  t <- p
  ts <- p*
} yield t +: ts

private def prefixSuffixKind = AnyRef()

def [I, T](p1: Parser[I, T]) :>> (p2: => Parser[I, T]) : Parser[I, T] = new Parser[I, T] {
  override def kind = prefixSuffixKind
  override def parseImpl(input: ParserState[I]) = for {
    r1 <- p1.parse(input)
    r2 <- p2.parse(input)
  } yield r2
}

def [I, T](p1: Parser[I, T]) <<: (p2: => Parser[I, T]) : Parser[I, T] = new Parser[I, T] {
  override def kind = prefixSuffixKind
  override def parseImpl(input: ParserState[I]) = for {
    r1 <- p1.parse(input)
    r2 <- p2.parse(input)
  } yield r1
}

def [I, F, T](fnP: Parser[I, F => T]) $ (fP: => Parser[I, F]) : Parser[I, T] = for {
  fn <- fnP
  f <- fP
} yield fn(f)

def [I, F1, F2, T](fnP: Parser[I, (F1, F2) => T]) $ (
  f1P: => Parser[I, F1],
  f2P: => Parser[I, F2],
  ) : Parser[I, T] = for {
  fn <- fnP
  f1 <- f1P
  f2 <- f2P
} yield fn(f1, f2)

def [I, F1, F2, F3, T](fnP: Parser[I, (F1, F2, F3) => T]) $ (
  f1P: => Parser[I, F1],
  f2P: => Parser[I, F2],
  f3P: => Parser[I, F3],
  ) : Parser[I, T] = for {
  fn <- fnP
  f1 <- f1P
  f2 <- f2P
  f3 <- f3P
} yield fn(f1, f2, f3)

def [I, F1, F2, F3, F4, T](fnP: Parser[I, (F1, F2, F3, F4) => T]) $ (
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
