package io.github.tgeng.fluentassert

import org.junit.Assert._

def [T] (subject: T) should(assertion: Assertion[T]) = assertion(subject)

def equal[T](target: T) = Assertion[T](t => (s"equal ${target.description}", t == target), true)

class Assertion[T](messageAndPredicate: T => (String, Boolean), objective: Boolean) {

  def apply(subject: T) = {
    val (message, predicate) = messageAndPredicate(subject)
    if(predicate != objective) {
      val not = if (objective) "" else "not "
      fail(s"""
      |Expect
      |  ${subject.description}
      |${not}to $message
      """.stripMargin)
    }
  }

  def unary_! = Assertion[T](messageAndPredicate, !objective)
}

private def[T] (t: T) description = s"<${t.getClass.getName}> $t"
