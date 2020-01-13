package io.github.tgeng.zealot.tt.frontend

import scala.collection.mutable.ArrayBuffer
import io.github.tgeng.zealot.common._
import io.github.tgeng.zealot.common.OptionSugar._
import io.github.tgeng.zealot.tt.core.Binder
import io.github.tgeng.zealot.tt.core.HasBinder
import io.github.tgeng.zealot.tt.core.Context
import io.github.tgeng.zealot.tt.core.Term
import io.github.tgeng.zealot.tt.core.Redux
import io.github.tgeng.zealot.tt.core.Value
import io.github.tgeng.zealot.tt.core.Traverser
import io.github.tgeng.zealot.tt.core.traverse

def (t: Term) toFTerm()(given ctx: Context[Binder]) : FTerm = {
  t.traverse(new Traverser[Binder](b => b.binder) {
    override def visitBinder(b: Binder)(given ctx: Context[Binder]) = {
      b.interferers.clear()
    }
  })

  val allBinders = ArrayBuffer[Binder]()
  t.traverse(new Traverser[Binder](b => b.binder) {
    override def visitRef(ref: Term.Ref)(given ctx: Context[Binder]) = {
      val b = ctx(ref.ref).orThrow(IllegalStateException(s"Reference ${ref.ref} is invalid in context\n$ctx"))
      allBinders += b
      ctx.inner(ref.ref).foreach( _.link(b))
    }
  })

  val conflictingBinders = ArrayBuffer[(Binder, Int)]()
  for (b <- allBinders) {
    val numConflictingBinders : Int = b.interferers.count(_.name == b.name)
    if (numConflictingBinders != 0) {
      conflictingBinders += ((b, numConflictingBinders))
    }
  }

  conflictingBinders
    .sortBy((b, count) => -count)
    .foreach{(b, _) =>
      b.name = generateUniqueName(b.name.ifEmpty("_"), b.interferers.map(_.name).toSet)
    }

  t.toFTermDirectly()(given Context())
}

private def generateUniqueName(proposal: String, existingNames: Set[String]): String = {
  val lastDigitIndex = proposal.lastIndexWhere(Character.isDigit)
  var (prefix, index) = if (lastDigitIndex == -1) {
    (proposal, 0)
  } else {
    val (prefix, indexString) = proposal.splitAt(lastDigitIndex)
    (prefix, indexString.toInt)
  }

  var newName = prefix + (index + 1)
  while(existingNames.contains(newName)) {
    index += 1
    newName = prefix + index
  }
  newName
}

// Converts a Term to FTerm directly, assuming there are no name conflicts.
private def (t: Term) toFTermDirectly()(given ctx: Context[String]) : FTerm = {
  import Term._
  import Redux._
  import Value._
  import FBuilder.{given, _}
  import scala.language.implicitConversions
  t match {
    case Ref(r) => ctx(r).map(_.ref)
      .orThrow(IllegalArgumentException(s"Reference $r is invalid in context\n$ctx"))
    case Val(v) => v match {
      case Set(l) => set(l)
      case v@Pi(dom, cod) => {
        val fDom = dom.toFTermDirectly()
        val fCod = (v.binder.name :: ctx) {
          cod.toFTermDirectly()
        }
        (v.binder.name, fDom) ->: fCod
      }
      case v@Lam(body) => (v.binder.name :: ctx) {
        \(v.binder.name) =>: body.toFTermDirectly()
      }
      case v@Sig(a, b) => {
        val fA = a.toFTermDirectly()
        val fB = (v.binder.name :: ctx) {
          b.toFTermDirectly()
        }
        (v.binder.name, fA) &: fB
      }
      case Pair(a, b) => (a.toFTermDirectly(), b.toFTermDirectly())
      case Unit => unit
      case Star => star
    }
    case Rdx(r) => r match {
      case App(fn, arg) => (fn.toFTermDirectly())(arg.toFTermDirectly())
      case Prj1(pair) => p1(pair.toFTermDirectly())
      case Prj2(pair) => p2(pair.toFTermDirectly())
    }
  }
}
