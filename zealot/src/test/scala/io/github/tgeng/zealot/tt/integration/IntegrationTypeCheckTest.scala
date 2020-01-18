package io.github.tgeng.zealot.tt.integration

import org.junit.Assert._
import org.junit.Test
import io.github.tgeng.fluentassert._
import io.github.tgeng.zealot.tt.core._
import io.github.tgeng.zealot.tt.frontend._

class IntegrationTypeCheckTest {

  given ctx : TypeContext = TypeContext()

  @Test
  def `basic cases` = {
    "Unit" :> "Set0"
    "()" :> "Unit"
    "Set0" :> "Set1"
    "Set1" :> "Set2"
    "(Unit, Set0)" :< "Set0 & Set1"
    "Set0 & Set1" :> "Set2"
    "\\x => x" :< "Unit -> Unit"
    "Set0 -> Set1" :> "Set2"
    "\\A, x => x" :< "(A: Set0) -> (x: A) -> A"

    "\\A, B, C, f, g, x => f(g x)" :<
    """
      (A: Set0) ->
      (B: Set0) ->
      (C: B -> Set0) ->
      ((x: B) -> C x) ->
      (g: A -> B) ->
      (x: A) ->
      C (g x)
    """
  }

  def (t1: String) :< (t2: String)(given ctx: TypeContext) =
    testStringAsTerm(t1, t2){ (t1, t2, ctx) =>
      t1 should checkWithType(t2)
    }

  def (t1: String) :> (t2: String)(given ctx: TypeContext) =
    testStringAsTerm(t1, t2){ (t1, t2, ctx) =>
      t1 should haveInferredType(t2)
    }

  private def testStringAsTerm(s1: String, s2: String)(op: (Term, Term, TypeContext) => Unit)(given ctx: TypeContext) = {
    given deBruijnContext: DeBruijnContext()
    (for {
      ft1 <- fTermParser.parse(s1)
      t1 <- ft1.toTerm()
      ft2 <- fTermParser.parse(s2)
      t2 <- ft2.toTerm()
    } yield op(t1, t2, ctx)) match {
      case Right(_) => ()
      case Left(e: Exception) => throw e
      case Left(e) => fail(e.toString)
    }
  }
}
