package io.github.tgeng.zealot.tt.core

import Term._
import Reference._
import Value._
import Redux._

trait Traverser[T](binderToT: Binder => T) {

  def visitTerm(t: Term)(given ctx: Context[T]): scala.Unit = t match {
    case r@Ref(_) => visitRef(r)
    case v@Val(_) => visitVal(v)
    case r@Rdx(_) => visitRdx(r)
  }

  def visitRef(r: Ref)(given ctx: Context[T]) : scala.Unit = r.ref match {
    case i@Idx(_) => visitIdx(i)
    case n@Num(_) => visitNum(n)
  }

  def visitIdx(i: Idx)(given ctx: Context[T]) : scala.Unit = ()

  def visitNum(n: Num)(given ctx: Context[T]) : scala.Unit = ()

  def visitVal(v: Val)(given ctx: Context[T]) : scala.Unit = v.value match {
    case s@Set(_) => visitSet(s)
    case p@Pi(_, _) => {
      visitBinder(p)
      visitPi(p)
    }
    case l@Lam(_) => {
      visitBinder(l)
      visitLam(l)
    }
    case s@Sig(_, _) => {
      visitBinder(s)
      visitSig(s)
    }
    case p@Pair(_, _) => visitPair(p)
    case Unit => visitUnit()
    case Star => visitStar()
  }

  def visitBinder(b: Binder)(given ctx: Context[T]) : scala.Unit = ()

  def visitSet(s: Set)(given ctx: Context[T]) : scala.Unit = ()

  def visitPi(p: Pi)(given ctx: Context[T]) : scala.Unit = {
    visitTerm(p.dom)
    (binderToT(p) :: ctx) {
      visitTerm(p.cod)
    }
  }

  def visitLam(l: Lam)(given ctx: Context[T]) : scala.Unit = (binderToT(l) :: ctx) {
    visitTerm(l.body)
  }

  def visitSig(s: Sig)(given ctx: Context[T]) : scala.Unit = {
    visitTerm(s.fstTy)
    (binderToT(s) :: ctx) {
      visitTerm(s.sndTy)
    }
  }

  def visitPair(p: Pair)(given ctx: Context[T]) : scala.Unit = {
    visitTerm(p.fst)
    visitTerm(p.snd)
  }

  def visitUnit()(given ctx: Context[T]) : scala.Unit = ()

  def visitStar()(given ctx: Context[T]) : scala.Unit = ()

  def visitRdx(r: Rdx)(given ctx: Context[T]) : scala.Unit = r.rdx match {
    case a@App(_, _) => visitApp(a)
    case p@Prj1(_) => visitPrj1(p)
    case p@Prj2(_) => visitPrj2(p)
  }

  def visitApp(a: App[Term])(given ctx: Context[T]) : scala.Unit = {
    visitTerm(a.fn)
    visitTerm(a.arg)
  }

  def visitPrj1(p: Prj1[Term])(given ctx: Context[T]) : scala.Unit = {
    visitTerm(p.pair)
  }

  def visitPrj2(p: Prj2[Term])(given ctx: Context[T]) : scala.Unit = {
    visitTerm(p.pair)
  }
}

def [T](t: Term) traverse(traverser: Traverser[T]) : scala.Unit = traverser.visitTerm(t)(given Context())
