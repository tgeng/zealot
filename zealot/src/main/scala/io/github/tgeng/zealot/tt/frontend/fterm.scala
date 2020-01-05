package io.github.tgeng.zealot.tt.frontend

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import io.github.tgeng.zealot.common._
import io.github.tgeng.zealot.tt.core.Term
import io.github.tgeng.zealot.tt.core.Builder


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

  def ++=(names: Traversable[String]) = this.+=(names.toSeq : _*)

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

def (t: Term) toFTerm() : FTerm = {
  throw UnsupportedOperationException()
}

def (t: Term) toFTermDirectly() : FTerm = {
  import FBuilder.{given, _}
  throw UnsupportedOperationException()
}
