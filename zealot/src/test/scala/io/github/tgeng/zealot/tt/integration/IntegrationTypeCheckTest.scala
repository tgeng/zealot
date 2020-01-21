package io.github.tgeng.zealot.tt.integration

import org.junit.Assert._
import org.junit.Test
import io.github.tgeng.fluentassert._
import io.github.tgeng.zealot.tt.core._
import io.github.tgeng.zealot.tt.frontend._

class IntegrationTypeCheckTest {

  given ctx : TypeContext = TypeContext()
  given glbCtx: GlobalContext = GlobalContext()

  @Test
  def `basic cases` = {
    ".zealot.Unit" :> ".zealot.Set"
    "()" :> ".zealot.Unit"
    ".zealot.Set" :> ".zealot.Set1"
    ".zealot.Set1" :> ".zealot.Set2"
    "(.zealot.Unit, .zealot.Set0)" :< ".zealot.Set0 & .zealot.Set1"
    ".zealot.Set & .zealot.Set1" :> ".zealot.Set2"
    "\\x => x" :< ".zealot.Unit -> .zealot.Unit"
    ".zealot.Set -> .zealot.Set1" :> ".zealot.Set2"
    "\\A, x => x" :< "(A: .zealot.Set) -> (x: A) -> A"

    "\\A, B, C, f, g, x => f(g x)" :<
    """
      (A: .zealot.Set) ->
      (B: .zealot.Set) ->
      (C: B -> .zealot.Set) ->
      ((x: B) -> C x) ->
      (g: A -> B) ->
      (x: A) ->
      C (g x)
    """
  }

  @Test
  def `global reference` = {
    import io.github.tgeng.zealot.tt.core.Builder.{given, _}

    ".zealot.Unit" :> ".zealot.Set"
    ".zealot.Star" :> ".zealot.Unit"
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
      case Left(e) => fail(s"Testing\n  $s1\nand\n  $s2\n" + e.toString)
    }
  }
}
