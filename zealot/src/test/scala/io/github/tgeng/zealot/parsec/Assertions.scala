package io.github.tgeng.zealot.parsec

import io.github.tgeng.zealot.common._
import io.github.tgeng.fluentassert._

def failWithMessage[T](message: String) = Assertion[Either[ParserError[Char], T]] { (t, objective) =>
  val trimmedMessage = message.trim.replaceAll("\n +", "\n")
  t match {
    case Right(t) if objective => Some(s"to fail but it succeeds with\n  $t")
    case Right(_) => None
    case Left(e) => (e.toString() == trimmedMessage, objective) match {
      case (true, false) => Some(s"to not fail with message\n  ${trimmedMessage.indented(2)}")
      case (false, true) => Some(s"to fail with message\n  ${trimmedMessage.indented(2)}\nbut it fails with message\n  ${e.toString().indented(2)}")
      case _ => None
    }
  }
}
