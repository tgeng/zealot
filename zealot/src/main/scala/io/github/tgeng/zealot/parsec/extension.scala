package io.github.tgeng.zealot.parsec

import scala.collection.mutable.ArrayBuffer

val empty: Parser[Any, Unit] = pure(()) withStrongName "<empty>"
def any[I] : Parser[I, I] = satisfy[I](_ => true) withStrongName "<any>"
val eof : Parser[Any, Unit] = not(any) withStrongName "<eof>"
val skip : Parser[Any, Unit] = satisfy[Any](_ => true).map(_ => ()) withStrongName "<skip>"
def anyOf[I](candidates : Seq[I]) : Parser[I, I] = satisfy[I](candidates.contains(_)) withStrongName s"<anyOf{${candidates.mkString(", ")}}>"

private val starKind = Kind(9, "*")

def [I, T](p: Parser[I, T])* = new Parser[I, IndexedSeq[T]](){
  override def kind : Kind = starKind
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

def [I, T](p: Parser[I, T])+ : Parser[I, IndexedSeq[T]] = {
  val kind = Kind(9, "+")
  (for {
  t <- p
  ts <- p*
} yield t +: ts) withDetailAndKind(p.name(kind) + "+", kind)
}

def [I, T](p: Parser[I, T])? : Parser[I, Option[T]] = {
  val kind = Kind(9, "?")
  (p.map(Some[T]) | empty.map(_ => None)) withDetailAndKind(
    p.name(kind) + "?",
    kind
  )
}

private val repeatKind = Kind(8, "n*_")

def [I, T](count: Int) *(p: Parser[I, T]) = new Parser[I, IndexedSeq[T]] {
  override def kind : Kind = repeatKind
  override def detailImpl = s"$count * " + p.name(kind)
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I], IndexedSeq[T]] = {
    val position = input.position;
    val result = new ArrayBuffer[T](count)
    var i = 0
    while (i < count) {
      i += 1
      p.parse(input) match {
        case Right(t) => result += t
        case Left(e) => return Left(ParserError(position, this, e))
      }
    }
    Right(result.toIndexedSeq)
  }
}


def [I, T](p: Parser[I, T]) sepBy1 (s: Parser[I, ?]) : Parser[I, IndexedSeq[T]] = {
  val sepKind = Kind(0, "sepBy1")
  p +: ((s >> p)*) withDetailAndKind (
  s"${p.name(sepKind)} sepBy1 ${s.name(sepKind)}",
  sepKind)
}

def [I, T](p: Parser[I, T]) sepBy (s: Parser[I, ?]) : Parser[I, IndexedSeq[T]] = {
  val sepKind = Kind(0, "sepBy")
  (p.sepBy1(s) | empty.map(_ => IndexedSeq.empty)) withDetailAndKind (
  s"${p.name(sepKind)} sepBy ${s.name(sepKind)}",
  sepKind)
}

def [I, T](p: Parser[I, T]) sepByN (count: Int) (s: Parser[I, ?]) : Parser[I, IndexedSeq[T]] = {
  val sepKind = Kind(0, "sepByN")
  count match {
    case 0 => empty.map{_ => IndexedSeq.empty[T]}
    case 1 => p.map(IndexedSeq[T](_))
    case n => p +: (n - 1) * (s >> p)
  } withDetailAndKind (
    s"${p.name(sepKind)} sepByN($count) ${s.name(sepKind)}",
    sepKind)
}

def [I, T](elemParser: Parser[I, T]) chainedLeftBy(opParser: Parser[I, (T, T) => T]) : Parser[I, T] = {
  val chainKind = Kind(0, "chainedLeftBy")
  foldLeft(elemParser, opParser, elemParser)
  .withDetailAndKind(
    s"${elemParser.name(chainKind)} chainedLeftBy ${opParser.name(chainKind)}",
    chainKind)
}

def [I, T](elemParser: Parser[I, T]) chainedRightBy(opParser: Parser[I, (T, T) => T]) : Parser[I, T] = {
  val chainKind = Kind(0, "chainedLeftBy")
  foldRight(elemParser, opParser, elemParser)
  .withDetailAndKind(
  s"${elemParser.name(chainKind)} chainedRightBy ${opParser.name(chainKind)}",
  chainKind)
}

def foldLeft[I, L, R](leftMostParser: Parser[I, L], opParser: Parser[I, (L, R) => L], elemParser: Parser[I, R]) : Parser[I, L] = {
  val foldKind = Kind(10, "foldLeft")
  (for {
    first <- leftMostParser
    rest <- (for {
      op <- opParser
      elem <- elemParser
    } yield (op, elem))*
  } yield rest.foldLeft(first)((acc, p) => p._1(acc, p._2))).withDetailAndKind(
    s"foldLeft{${leftMostParser.name(foldKind)} (${opParser.name(foldKind)} ${elemParser.name(foldKind)})*}",
    foldKind)
}

def foldRight[I, L, R](elemParser: Parser[I, L], opParser: Parser[I, (L, R) => R], rightMostParser: Parser[I, R]) : Parser[I, R] = {
  val foldKind = Kind(10, "foldRight")
  (for {
    front <- ((for {
      elem <- elemParser
      op <- opParser
    } yield (op, elem))*)
    last <- rightMostParser
  } yield front.foldRight(last)((p, acc) => p._1(p._2, acc))).withDetailAndKind(
    s"foldRight{(${elemParser.name(foldKind)} ${opParser.name(foldKind)})* ${rightMostParser.name(foldKind)}}",
    foldKind)
}

val prefixSuffixKind = Kind(4, "prefixSuffix")

def [I, T](p1: Parser[I, ?]) >> (p2: Parser[I, T]) : Parser[I, T] = (for {
  _ <- p1
  t <- p2
} yield t) withDetailAndKind (
  p1.name(prefixSuffixKind) + " >> " + p2.name(prefixSuffixKind),
  prefixSuffixKind)

def [I, T](p1: Parser[I, T]) << (p2: Parser[I, ?]) : Parser[I, T] = (for {
  t <- p1
  _ <- p2
} yield t) withDetailAndKind (
  p1.name(prefixSuffixKind) + " << " + p2.name(prefixSuffixKind),
  prefixSuffixKind)

def applyKind = Kind(10, "apply")

def [I, F, T](fnP: Parser[I, F => T]) $(
  fP: => Parser[I, F]
  ) : Parser[I, T] = (for {
  fn <- fnP
  f <- fP
} yield fn(f)) withDetailAndKind(
  s"${fnP.name(applyKind)} $$ (${fP.name()})",
  applyKind)

def [I, F1, F2, T](fnP: Parser[I, (F1, F2) => T]) $ (
  f1P: => Parser[I, F1],
  f2P: => Parser[I, F2],
  ) : Parser[I, T] = (for {
  fn <- fnP
  f1 <- f1P
  f2 <- f2P
} yield fn(f1, f2)) withDetailAndKind (
  s"${fnP.name(applyKind)} $$ (${f1P.name()}, ${f2P.name()})",
  applyKind)

def [I, F1, F2, F3, T](fnP: Parser[I, (F1, F2, F3) => T]) $ (
  f1P: => Parser[I, F1],
  f2P: => Parser[I, F2],
  f3P: => Parser[I, F3],
  ) : Parser[I, T] = (for {
  fn <- fnP
  f1 <- f1P
  f2 <- f2P
  f3 <- f3P
} yield fn(f1, f2, f3)) withDetailAndKind (
  s"${fnP.name(applyKind)} $$ (${f1P.name()}, ${f2P.name()}, ${f3P.name()})",
  applyKind)

def [I, F1, F2, F3, F4, T](fnP: Parser[I, (F1, F2, F3, F4) => T]) $ (
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
  s"${fnP.name(applyKind)} $$ (${f1P.name()}, ${f2P.name()}, ${f3P.name()}, ${f4P.name()})",
  applyKind)

val prependAppendConcat = Kind(7, "prependAppendConcat")

def [I, T](p1: Parser[I, T]) +:+ (p2: Parser[I, T]) : Parser[I, IndexedSeq[T]] = (for {
  t1 <- p1
  t2 <- p2
} yield IndexedSeq(t1, t2)).withDetailAndKind(
  s"${p1.name(prependAppendConcat)} +:+ ${p2.name(prependAppendConcat)}",
  prependAppendConcat)

def [I, T, S <: IndexedSeq[T]](p1: Parser[I, T]) +: (p2: Parser[I, IndexedSeq[T]]) : Parser[I, IndexedSeq[T]] = (for {
  t <- p1
  ts <- p2
} yield t +: ts).withDetailAndKind(
  s"${p1.name(prependAppendConcat)} +: ${p2.name(prependAppendConcat)}",
  prependAppendConcat)

def [I, T](p1: Parser[I, IndexedSeq[T]]) :+ (p2: Parser[I, T]) : Parser[I, IndexedSeq[T]] = (for {
  ts <- p1
  t <- p2
} yield ts :+ t).withDetailAndKind(
  s"${p1.name(prependAppendConcat)} :+ ${p2.name(prependAppendConcat)}",
  prependAppendConcat)

def [I, T](p1: Parser[I, IndexedSeq[T]]) ++ (p2: Parser[I, IndexedSeq[T]]) : Parser[I, IndexedSeq[T]] = (for {
  ts1 <- p1
  ts2 <- p2
} yield ts1 ++ ts2).withDetailAndKind(
  s"${p1.name(prependAppendConcat)} ++ ${p2.name(prependAppendConcat)}",
  prependAppendConcat)
