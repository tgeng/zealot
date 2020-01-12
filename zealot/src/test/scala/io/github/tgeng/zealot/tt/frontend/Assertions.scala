package io.github.tgeng.zealot.tt.frontend

import scala.collection.immutable.Seq
import io.github.tgeng.fluentassert._
import io.github.tgeng.zealot.common._
import io.github.tgeng.zealot.tt.core.Binder
import io.github.tgeng.zealot.tt.core.Context
import io.github.tgeng.zealot.tt.core.Term

def haveDeBruijnTerm(target: Term)(given ctx: DeBruijnContext) = AssertionBase[FTerm] { (ft, objective) =>
  ft.toTerm() match {
    case Right(t) => (objective, t == target) match {
      case (true, false) => Some(s"to translate to De Bruijn term\n  $target\nbut it translates to\n  $t")
      case (false, true) => Some(s"to not translate to De Bruijn term\n  $target")
      case _ => None
    }
    case Left(e) => Some(s"to be translatable but it fails with error\n  ${e.getMessage().indented(2)}\nat when translating\n  ${e.offender}")
  }
}

def haveFrontendTerm(target: FTerm)(given ctx: Context[Binder]) = AssertionBase[Term] { (t, objective) =>
  val ft = t.toFTerm()
  (ft == target, objective) match {
    case (false, true) => Some(s"to translate to frontend term\n  $target\nbut it translates to\n  $ft")
    case (true, false) => Some(s"to not translate to frontend term\n  $target")
    case _ => None
  }
}

def remainTheSameTermAfterRoundTrip = AssertionBase[Term] {(t, objective) =>
  if (!objective) throw IllegalStateException("Wat? A term should always remain the same after round trip translatoin.")
  t.toFTerm()(given Context()).toTerm()(given DeBruijnContext()) match {
    case Right(translated) => t == translated match {
      case true => None
      case false => Some(s"to remain as the same term after round trip translation but it becomes\n  $translated")
    }
    case Left(e) => Some(s"to remain as the same term after round trip translation but it fails with message\n  ${e.getMessage()}")
  }
}

def remainTheSameFTermAfterRoundTrip = AssertionBase[FTerm] {(t, objective) =>
  if (!objective) throw IllegalStateException("Wat? A term should always remain the same after round trip translatoin.")
  t.toTerm()(given DeBruijnContext()).map(_.toFTerm()(given Context())) match {
    case Right(translated) => t == translated match {
      case true => None
      case false => Some(s"to remain as the same term after round trip translation but it becomes\n  $translated")
    }
    case Left(e) => Some(s"to remain as the same term after round trip translation but it fails with message\n  ${e.getMessage()}")
  }
}
