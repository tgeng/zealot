package io.github.tgeng.zealot.parsec

val rootKind = Kind(-1)

class ParserState[+I](val content: IndexedSeq[I], var position: Int, var commitPosition: Int)

case class ParserError[-I](
  val position: Int,
  val failureParser: Parser[I, ?],
  val cause: ParserError[I] | Null)

class Kind(val precedence: Int)

trait Parser[-I, +T]() {
  // The kind of this parser. This is used to merge parsers of the same kind during error reporting.
  val kind : Kind = Kind(10)
  def name(parentKind : Kind = rootKind) : String = detail(parentKind)
  def detail(parentKind : Kind = rootKind) : String =
    if (parentKind.precedence > kind.precedence) s"($detailImpl)"
    else detailImpl
  def detailImpl : String
  def parse(input: ParserState[I]) : Either[ParserError[I], T] = {
    val startPosition = input.position
    if (startPosition >= input.content.size) {
      return Left(ParserError(startPosition, this, null))
    }
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

private val mapFlatMapKind = Kind(3)

def [I, T, R](p: Parser[I, T]) map(f: T => R): Parser[I, R] = {
  new Parser[I, R] {
    override val kind = mapFlatMapKind
    override def detailImpl = p.name(kind)
    override def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, R] =
      p.parse(input).map(f)
  }
}

def [I, T, R](p: Parser[I, T]) flatMap(f: T => Parser[I, R]) : Parser[I, R] = {
  var nextParser : Parser[I, R] | Null = null
  new Parser[I, R] {
    override val kind = mapFlatMapKind
    override def detailImpl = {
      val np = nextParser
      p.name(kind) + " " + (if (np == null) "<?>" else np.name(kind))
    }
    override def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, R] =
      p.parse(input).flatMap(t => {
          val p = f(t)
          nextParser = p
          p.parse(input)
        })
  }
}

def [I, T](p: Parser[I, T]) withNameFn(nameTransformer: String => String) : Parser[I, T] = new Parser[I, T] {
  override def name(parentKind : Kind) = nameTransformer(p.name())
  override def detailImpl = p.name()
  override def parse(input: ParserState[I]) : Either[ParserError[I], T] = p.parse(input) match {
    case Left(ParserError(position, failureParser, cause)) => Left(ParserError(position, this, cause))
    case t@_ => t
  }
  override def parseImpl(input: ParserState[I]) = throw UnsupportedOperationException()
}

def [I, T](p: Parser[I, T]) withName(newName: String) : Parser[I, T] = p.withNameFn(_ => newName)

def [I, T](p: Parser[I, T])unary_! = new Parser[I, T] {
  override val kind = Kind(5)
  override def detailImpl = p.name(kind)
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
  override def detailImpl = "<pos>"
  override def parseImpl(input: ParserState[Any]) = Right(input.position)
}

def pure[I, T](t: T) = new Parser[I, T] {
  override def detailImpl = "<pure>"
  override def parseImpl(input: ParserState[I]) = Right(t)
}

def not[I](p: Parser[I, ?]) = new Parser[I, Unit] {
  override val kind = Kind(5)
  override def detailImpl = "not " + p.name(kind)
  override def parse(input: ParserState[I]) = {
    val position = input.position
    val commitPosition = input.commitPosition
    val result = p.parse(input) match {
      case Right(_) => Left(ParserError(position, this, null))
      case Left(_) => Right(())
    }
    input.position = position
    input.commitPosition = commitPosition
    result
  }
  override def parseImpl(input: ParserState[I]) = throw UnsupportedOperationException()
}

def satisfy[I](predicate: I => Boolean) = new Parser[I, I] {
  override def detailImpl = "<satisfy>"
  override def parseImpl(input: ParserState[I]) = {
    val position = input.position
    val t = input.content(position)
    input.position += 1
    if (predicate(t)) {
      Right(t)
    } else {
      Left(ParserError(position, this, null))
    }
  }
}

private val orKind = Kind(1)

def [I, T](p1: Parser[I, T]) | (p2: => Parser[I, T]) : Parser[I, T] = new Parser[I, T] {
  override val kind = orKind
  override def detailImpl = p1.name(kind) + " | " + p2.name(kind)
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
