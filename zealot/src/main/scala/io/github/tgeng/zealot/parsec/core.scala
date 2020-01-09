package io.github.tgeng.zealot.parsec

private val mapFlatMapKind = AnyRef()

trait Parser[-I, +T]() {
  // The kind of this parser. This is used to merge parsers of the same kind during error reporting.
  def kind : Any = this
  def parse(input: ParserState[I]) : Either[ParserError[I], T] = {
    val startPosition = input.position
    val result = parseImpl(input)
    result match {
      case Right(t) => Right(t)
      case Left(e) => if (e != null && e.failureParser.kind == kind) {
        // Skip ParserError e since it's from the same kind of parser of this one.
        Left(ParserError(startPosition, this, e.cause))
      } else {
        Left(ParserError(startPosition, this, e))
      }
    }
  }

  protected def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, T]
}

def [I, T, R](p: Parser[I, T]) map(f: T => R): Parser[I, R] = {
  new Parser[I, R] {
    override def kind = mapFlatMapKind
    override def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, R] =
      p.parse(input).map(f)
  }
}

def [I, T, R](p: Parser[I, T]) flatMap(f: T => Parser[I, R]) : Parser[I, R] = {
  var nextParser : Parser[I, R] | Null = null
  new Parser[I, R] {
    override def kind = mapFlatMapKind
    override def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, R] =
      p.parse(input).flatMap(t => {
          val p = f(t)
          nextParser = p
          p.parse(input)
        })
  }
}

class ParserState[+I](val content: IndexedSeq[I], var position: Int, var commitPosition: Int)

case class ParserError[-I](val position: Int, failureParser: Parser[I, ?], val cause: ParserError[I] | Null)

def [I, T](p: Parser[I, T]) withNameFn(nameTransformer: String => String) = new Parser[I, T] {
  override def parse(input: ParserState[I]) : Either[ParserError[I], T] = p.parse(input) match {
    case Left(ParserError(position, failureParser, cause)) => Left(ParserError(position, this, cause))
    case t@_ => t
  }
  override def parseImpl(input: ParserState[I]) = throw UnsupportedOperationException()
}

def [I, T](p: Parser[I, T]) withName(newName: String) = p.withNameFn(_ => newName)

def [I, T](p: Parser[I, T])unary_! = new Parser[I, T] {
  override def kind = p.kind
  override def parse(input: ParserState[I]) : Either[ParserError[I], T] = {
    input.commitPosition = input.position
    p.parse(input) match {
        case Left(ParserError(position, failureParser, cause)) => Left(ParserError(position, this, cause))
        case t@_ => t
    }
  }
  override def parseImpl(input: ParserState[I]) = throw UnsupportedOperationException()
}

private def alternativeKind = AnyRef()

val position = new Parser[Any, Int] {
  override def parseImpl(input: ParserState[Any]) = Right(input.position)
}

def pure[I, T](t: T, aName: String = "<val>") = new Parser[I, T] {
  override def kind = t
  override def parseImpl(input: ParserState[I]) = Right(t)
}

val empty: Parser[Any, Unit] = pure((), "<empty>")

val eos = new Parser[Any, Unit] {
  override def parseImpl(input: ParserState[Any]) =
    if (input.position == input.content.size) Right(())
    else Left(ParserError(input.position, this, null))
}

def [I, T](p1: Parser[I, T]) | (p2: => Parser[I, T]) : Parser[I, T] = new Parser[I, T] {
  override def kind = alternativeKind
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, T] = {
    val startPosition = input.position
    p1.parse(input) match {
      case Left(_) if (startPosition >= input.commitPosition) => {
        input.position = startPosition
        p2.parse(input)
      }
      case t1@_ => t1
    }
  }
}
