package io.github.tgeng.fluentassert

import org.junit.Assert._

def [T] (subject: T) should(assertion: Assertion[T]) = assertion(subject)

def equal[T](target: T) = Assertion[T] {(t, objective) => 
  if (objective && t != target) {
    Some(s"to equal ${target.description}")
  } else if (!objective && t == target) {
    Some("to not equal ${target.description}")
  } else {
    None
  }
}

class Assertion[T](failureMessage: (T, Boolean) => Option[String], objective : Boolean = true) {

  def apply(subject: T) = {
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

  def unary_! = Assertion[T](failureMessage, !objective)
}

private def[T] (t: T) description = s"<${t.getClass.getName}> $t"
