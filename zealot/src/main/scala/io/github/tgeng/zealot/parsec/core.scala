package io.github.tgeng.zealot.parsec

val rootKind = Kind(-1, "root")

inline def parser[I, T](p: Parser[I, T]) = p

class ParserState[+I](val content: IndexedSeq[I], var position: Int, var commitPosition: Int)

case class ParserError[-I](
  val position: Int,
  val failureParser: Parser[I, ?],
  val cause: ParserError[I] | Null) {
    override def toString() : String = {
      val failureParserName = failureParser.name()
      val failureParserDetail = failureParser.detail()
      val detail = if (failureParserName == failureParserDetail) {
        ""
      } else {
        " := " + failureParserDetail
      }
      (if (cause == null) {
        ""
      } else {
        cause.toString() + "\n"
      }) + s"$position: $failureParserName" + detail
    }
  }

case class Kind(val precedence: Int, val name: String)

trait Parser[-I, +T] {
  def kind : Kind
  def name(parentKind : Kind = rootKind) : String = detail(parentKind)
  def detail(parentKind : Kind = rootKind) : String = {
    if (parentKind.precedence > kind.precedence) s"($detailImpl)"
    else detailImpl
  }
  def detailImpl : String

  final def parse(input: IndexedSeq[I]) : Either[ParserError[I], T] = parse(ParserState(input, 0, 0))

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

  override def toString() = s"Parser{${name()}}"
}

def [I, T, R](p: Parser[I, T]) map(f: T => R): Parser[I, R] = {
  new Parser[I, R] {
    override def kind : Kind = p.kind
    override def name(k: Kind) = p.name(k)
    override def detailImpl = p.detailImpl
    override def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, R] =
      p.parse(input).map(f)
  }
}

private val flatMapKind = Kind(5, "flatMap")

def [I, T, R](p: Parser[I, T]) flatMap(f: T => Parser[I, R]) : Parser[I, R] = {
  var nextParser : Parser[I, R] | Null = null
  new Parser[I, R] {
    override def kind : Kind = flatMapKind
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

def [I, T](p: Parser[I, T]) withDetailFnAndKind(detailTransformer: String => String, newKind : Kind) : Parser[I, T] = new Parser[I, T] {
  override def kind : Kind = newKind
  override def detailImpl = detailTransformer(p.detailImpl)
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, T] = p.parse(input) match {
    case Left(ParserError(position, failureParser, cause)) => Left(cause)
    case t@_ => t
  }
}

def [I, T](p: Parser[I, T]) withDetailAndKind(newDetail: String, newKind: Kind) : Parser[I, T] = p.withDetailFnAndKind(_ => newDetail, newKind)

def [I, T](p: => Parser[I, T]) withName(newName: String) : Parser[I, T] = p.withNameAndDetail(newName, null)

def [I, T](p: => Parser[I, T]) withStrongName(newName: String) : Parser[I, T] = p.withNameAndDetail(newName, newName)

private def [I, T](p: => Parser[I, T]) withNameAndDetail(newName: String, newDetail: String|Null) : Parser[I, T] = new Parser[I, T] {
  override def kind : Kind = p.kind
  override def name(parentKind: Kind): String = newName
  override def detailImpl = if(newDetail == null) p.detailImpl else newDetail
  override def parseImpl(input: ParserState[I]) = p.parse(input)
}

private val scopedKind = Kind(10, "scoped")

def scoped[I, T](p: Parser[I, T]) = new Parser[I, T] {
  override def kind : Kind = scopedKind
  override def detailImpl = s"{ ${p.name()} }"
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I], T] = {
    val commitPosition = input.commitPosition
    val result = p.parse(input);
    input.commitPosition = commitPosition
    result
  }
}

private val commitToKind = Kind(10, "!")

def [I, T](p: Parser[I, T])unary_! = new Parser[I, T] {
  override def kind : Kind = commitToKind
  override def detailImpl = "!" + p.name(kind)
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I], T] = {
    input.commitPosition = input.position
    p.parse(input) match {
        case Left(ParserError(position, failureParser, cause)) => Left(ParserError(position, this, cause))
        case t@_ => t
    }
  }
}

def [I, T](p: Parser[I, T])! = new Parser[I, T] {
  override def kind : Kind = commitToKind
  override def detailImpl = p.name(kind) + "!"
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I], T] = {
    p.parse(input) match {
        case Left(ParserError(position, failureParser, cause)) => Left(ParserError(position, this, cause))
        case t@_ => {
          input.commitPosition = input.position
          t
        }
    }
  }
}

private def positionKind = Kind(10, "position")

val position = new Parser[Any, Int] {
  override def kind : Kind = positionKind
  override def detailImpl = "<pos>"
  override def parseImpl(input: ParserState[Any]) = Right(input.position)
}

private def pureKind = Kind(10, "pure")

def pure[I, T](t: T, aName : String = "<pure>") = new Parser[I, T] {
  override def kind : Kind = pureKind
  override def detailImpl = aName
  override def parseImpl(input: ParserState[I]) = Right(t)
}

private val notKind = Kind(5, "not")

def not[I](p: Parser[I, ?]) = new Parser[I, Unit] {
  override def kind : Kind = notKind
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

private val predicateKind = Kind(10, "predicate")

def satisfy[I](predicate: I => Boolean) = new Parser[I, I] {
  override def kind : Kind = predicateKind
  override def detailImpl = "<satisfy>"
  override def parseImpl(input: ParserState[I]) = {
    val position = input.position
    if (position >= input.content.size) {
      return Left(ParserError(position, this, null))
    }
    val t = input.content(position)
    input.position += 1
    if (predicate(t)) {
      Right(t)
    } else {
      Left(ParserError(position, this, null))
    }
  }
}

private val orKind = Kind(1, "|")

def [I, T](p1: Parser[I, T]) | (p2: Parser[I, T]) : Parser[I, T] = new Parser[I, T] {
  override def kind : Kind = orKind
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

private val andKind = Kind(2, "&")

// '&' operator passes left result from left operand and ignores result from right operand.
// However, if right operand parses fails, the overall '&' parser fails too. This operator
// is useful to append other conditions to a given parser. For example, following parser
// matches any words other than keywords.
// ```
//   "[a-z]+".r & not ("keyword1" | "keyword2")
// ```
def [I, T](p: Parser[I, T]) & (cond: Parser[I, Any]): Parser[I, T] = new Parser[I, T] {
  override def kind : Kind = andKind
  override def detailImpl = p.name(kind) + " & " + cond.name(kind)
  override def parseImpl(input: ParserState[I]) : Either[ParserError[I] | Null, T] = {
    val startPosition = input.position
    val startCommitPosition = input.commitPosition
    p.parse(input) match {
      case t@Right(_) => {
        val finishPosition = input.position
        val commitPosition = input.commitPosition
        input.position = startPosition
        input.commitPosition = startCommitPosition
        val result = cond.parse(input) match {
          case Right(_) => t
          case Left(e) => Left(e)
        }
        input.position = finishPosition
        input.commitPosition = commitPosition
        result
      }
      case e@_ => e
    }
  }
}
