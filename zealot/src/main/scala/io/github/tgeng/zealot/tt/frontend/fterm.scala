package io.github.tgeng.zealot.tt.frontend

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import io.github.tgeng.zealot.common._
import io.github.tgeng.zealot.common.OptionSugar._
import io.github.tgeng.zealot.tt.core.Binder
import io.github.tgeng.zealot.tt.core.Builder
import io.github.tgeng.zealot.tt.core.Context
import io.github.tgeng.zealot.tt.core.Term
import io.github.tgeng.zealot.tt.core.Redux
import io.github.tgeng.zealot.tt.core.Value
import io.github.tgeng.zealot.tt.core.Traverser
import io.github.tgeng.zealot.tt.core.traverse


enum FTerm {
  case FRef(ref: FReference) extends FTerm
  case FVal(value: FValue) extends FTerm
  case FRdx(rdx: FRedux) extends FTerm
}

enum FReference {
  case FName(name: String)
}

enum FValue {
  case FSet(level: Int)
  case FPi(name: String, dom: FTerm, cod: FTerm)
  case FLam(name: String, body: FTerm)
  case FSig(name: String, fstTy: FTerm, sndTy: FTerm)
  case FPair(fst: FTerm, snd: FTerm)
  case FUnit()
  case FStar()
}

enum FRedux {
  case FApp(fn: FTerm, arg: FTerm)
  case FPrj1(pair: FTerm)
  case FPrj2(pair: FTerm)
}

import FTerm._
import FReference._
import FValue._
import FRedux._

def (ft: FTerm) toTerm()(given ctx: DeBruijnContext) : Either[DeBruijnizationError, Term] = {
  import Builder.{given, _}
  import io.github.tgeng.zealot.common.EitherSugar.given
  import scala.language.implicitConversions
  ft match {
    case FRef(FName(name)) => ctx(name) match {
      case Some(idx) => !idx
      case None => DeBruijnizationError(s"$name is not present in context\n  ${ctx.toString().indented(2)}", ft)
    }
    case FVal(v) => v match {
      case FSet(level) => set(level)
      case FPi(name, fDom, fCod) => for {
        dom <- fDom.toTerm()
        cod <- (name :: ctx) {
          fCod.toTerm()
        }
      } yield (name, dom) ->: cod
      case FLam(name, fBody) => for {
        body <- (name :: ctx)  {
          fBody.toTerm()
        }
      } yield lam(name, body)
      case FSig(name, fFstTy, fSndTy) => for {
        fstTy <- fFstTy.toTerm()
        sndTy <- (name :: ctx) {
          fSndTy.toTerm()
        }
      } yield (name, fstTy) x sndTy
      case FPair(fFst, fSnd) => for {
        fst <- fFst.toTerm()
        snd <- fSnd.toTerm()
      } yield (fst, snd)
      case FUnit() => unit
      case FStar() => *
    }
    case FRdx(r) => r match {
      case FApp(fFn, fArg) => for {
        fn <- fFn.toTerm()
        arg <- fArg.toTerm()
      } yield fn(arg)
      case FPrj1(fPair) => fPair.toTerm().map(p1)
      case FPrj2(fPair) => fPair.toTerm().map(p2)
    }
  }
}

class DeBruijnContext()  {
  private val binding: Map[String, ArrayBuffer[Int]] = Map()
  private var size: Int = 0

  def apply(name: String) : Option[Int] = binding.get(name).flatMap(_.lastOption).map(size - _)

  def +=(names: String*) = {
    for (name <- names) {
      size += 1
      binding.getOrElseUpdate(name, ArrayBuffer()) += size
    }
  }

  def ++=(names: Iterable[String]) = this.+=(names.toSeq : _*)

  def ::[T](name: String)(action: => T) : T = {
    this += name
    val result = action
    binding(name).dropRightInPlace(1)
    size -= 1
    result
  }

  override def toString() =
    if(binding.isEmpty) "<empty context>"
    else binding.map{(name, indices) =>
      s"$name: ${indices.map(size - _).mkString(" ")}"
    }.mkString("\n")
}

class DeBruijnizationError(msg: String, val offender: FTerm) extends Exception(msg);

def (t: Term) toFTerm()(given ctx: Context[Binder]) : FTerm = {
  t.traverse(new Traverser[Binder](b => b.binder) {
    override def visitBinder(b: Binder)(given ctx: Context[Binder]) = b.interferers.clear()
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
      b.name = generateUniqueName(b.name, b.interferers.map(_.name).toSet)
    }

  t.toFTermDirectly()(given Context())
}

var i = 0

private def generateUniqueName(proposal: String, existingNames: Set[String]): String = {
  // TODO(tgeng): use more fancy method to generate names.
  i += 1
  s"x_$i"
}

// Converts a Term to FTerm directly, assuming there are no name conflicts.
private def (t: Term) toFTermDirectly()(given ctx: Context[String]) : FTerm = {
  import Term._
  import Redux._
  import Value._
  import FBuilder.{given, _}
  import scala.language.implicitConversions
  t match {
    case Ref(r) => !ctx(r)
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
        (v.binder.name, fA) x fB
      }
      case Pair(a, b) => (a.toFTermDirectly(), b.toFTermDirectly())
      case Unit => unit
      case Star => *
    }
    case Rdx(r) => r match {
      case App(fn, arg) => (fn.toFTermDirectly())(arg.toFTermDirectly())
      case Prj1(pair) => p1(pair.toFTermDirectly())
      case Prj2(pair) => p2(pair.toFTermDirectly())
    }
  }
}
