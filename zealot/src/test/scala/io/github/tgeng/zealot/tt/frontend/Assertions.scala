package io.github.tgeng.zealot.tt.frontend

import scala.collection.immutable.Seq
import io.github.tgeng.fluentassert._
import io.github.tgeng.zealot.common._
import io.github.tgeng.zealot.tt.core.Term

def haveDeBruijnTerm(target: Term)(given ctx: DeBruijnContext) = Assertion[FTerm] { (ft, objective) =>
  ft.toTerm() match {
    case Right(t) => (objective, t == target) match {
      case (true, false) => Some(s"to translate to De Bruijn term\n  $target\nbut it translates to\n  $t")
      case (false, true) => Some(s"to not translate to De Bruijn term\n  $target")
      case _ => None
    }
    case Left(e) => Some(s"to be translatable but it fails with error\n  ${e.getMessage().indented(2)}\nat when translating\n  ${e.offender}")
  }
}
