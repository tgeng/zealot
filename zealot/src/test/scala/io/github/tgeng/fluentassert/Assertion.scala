package io.github.tgeng.fluentassert

import org.junit.Assert._

def [T] (subject: T) should(assertion: Assertion[T]) = assertion(subject, true)
def [T] (subject: T) shouldNot(assertion: Assertion[T]) = assertion(subject, false)
def [T] (subjects: Iterable[T]) shouldAll(assertion: Assertion[T]) = subjects.foreach(assertion(_, true))

trait Assertion[T] {
  def apply(subject: T, objective: Boolean) : Unit
}

class AssertionBase[T](failureMessage: (T, Boolean) => Option[String]) extends Assertion[T] {
  override def apply(subject: T, objective: Boolean) = {
    failureMessage(subject, objective) match {
      case None => ()
      case Some(msg) =>
      fail(s"""|Expect
               |  ${description(subject)}
               |$msg
               |""".stripMargin)
    }
  }
}

def equal[T](target: T) = AssertionBase[T] {(t, objective) =>
  if (objective && t != target) {
    Some(s"to equal\n  ${description(target)}")
  } else if (!objective && t == target) {
    Some("to not equal\n  ${target.description}")
  } else {
    None
  }
}

def succeedWith[L, R](target: R) : AssertionBase[Either[L, R]] = equal(Right(target))
def failWith[L, R](target: L) : AssertionBase[Either[L, R]] = equal(Left(target))

private def[T] (t: T) description = s"<${t.getClass.getName}> $t"
