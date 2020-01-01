package io.github.tgeng.fluentassert

import org.junit.Assert._

def [T] (subject: T) should(assertion: Assertion[T]) = assertion(subject, true)
def [T] (subject: T) shouldNot(assertion: Assertion[T]) = assertion(subject, false)

def equal[T](target: T) = Assertion[T] {(t, objective) =>
  if (objective && t != target) {
    Some(s"to equal ${target.description}")
  } else if (!objective && t == target) {
    Some("to not equal ${target.description}")
  } else {
    None
  }
}

class Assertion[T](failureMessage: (T, Boolean) => Option[String]) {

  def apply(subject: T, objective: Boolean) = {
    failureMessage(subject, objective) match {
      case None => ()
      case Some(msg) =>
      fail(s"""
      |Expect
      |  ${subject.description}
      |$msg
      """.stripMargin)
    }
  }
}

private def[T] (t: T) description = s"<${t.getClass.getName}> $t"
