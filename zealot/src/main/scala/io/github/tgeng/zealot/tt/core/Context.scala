package io.github.tgeng.zealot.tt.core

import scala.collection.mutable.ArrayBuffer
import io.github.tgeng.zealot.common._
import io.github.tgeng.zealot.tt.core.Neutral._
import io.github.tgeng.zealot.tt.core.Reference._
import io.github.tgeng.zealot.tt.core.Redux._
import io.github.tgeng.zealot.tt.core.Term._
import io.github.tgeng.zealot.tt.core.Whnf._
import io.github.tgeng.zealot.tt.core.Value._

type Type = Whnf

class Context[T] {
  private val content: ArrayBuffer[T] = ArrayBuffer()

  def apply(r: Reference) : Option[T] = r match {
    case Idx(i) => if (i < 0 || i >= content.size) Option.empty else Option(content(content.size - i - 1))
    case Num(n) => if (n < 0 || n >= content.size) Option.empty else Option(content(n))
  }

  // Gets elements "inside" the binder referenced by the given Reference.
  def inner(r: Reference) : Iterable[T] = r match {
    case Idx(i) => content.view.takeRight(i)
    case Num(n) => content.view.drop(n + 1)
  }

  def +=(tys: T*) = for (ty <- tys) content.append(processElement(ty))

  def ++=(tys: Iterable[T]) = this.+=(tys.toSeq : _*)

  def ::[R](ty: T) : (=> R) => R = processElement(ty) :: content

  def isIdxEqualNum(idx: Int, num: Int) = idx + num + 1 == content.size
  def idxToNum(idx: Int, offset: Int) : Int = content.size + offset - 1 - idx
  def snapshot : Seq[T] = content.toList

  protected def processElement(t: T) : T = t

  override def toString() = content.zipWithIndex.map{ (t, i) => s"  $i: $t" }.mkString("Context {\n", "\n", "\n}")
}

class TypeContext extends Context[Type] {
  override def processElement(ty: Type) = ty.replaceIdxWithNum(this)
}

def (t: Term) replaceIdxWithNum(ctx: Context[?]) = t.replaceIdxWithNumImpl(0)(given ctx)
def (t: Whnf) replaceIdxWithNum(ctx: Context[?]) = t.replaceIdxWithNumImpl(0)(given ctx)

private def (w: Whnf)replaceIdxWithNumImpl(offset: Int)(given ctx: Context[?]) : Whnf = w match {
  case Neu(n) => Neu(n.replaceIdxWithNumImpl(offset))
  case Whnf.Val(v) => Whnf.Val(v.replaceIdxWithNumImpl(offset))
}

private def (t: Term)replaceIdxWithNumImpl(offset: Int)(given ctx: Context[?]) : Term = t match {
  case Term.Ref(r) => Term.Ref(r.replaceIdxWithNumImpl(offset))
  case Term.Val(v) => Term.Val(v.replaceIdxWithNumImpl(offset))
  case Term.Rdx(r) => Term.Rdx(r.replaceIdxWithNumImpl(offset, _.replaceIdxWithNumImpl(offset)))
}

private def (n: Neutral)replaceIdxWithNumImpl(offset: Int)(given ctx: Context[?]) : Neutral = n match {
  case Neutral.Ref(r) => Neutral.Ref(r.replaceIdxWithNumImpl(offset))
  case Neutral.Rdx(r) => Neutral.Rdx(r.replaceIdxWithNumImpl(offset, _.replaceIdxWithNumImpl(offset)))
}

private def (v: Value)replaceIdxWithNumImpl(offset: Int)(given ctx: Context[?]) : Value = v match {
  case v@Pi(a, b) => Pi(a.replaceIdxWithNumImpl(offset), b.replaceIdxWithNumImpl(offset + 1))(v.binder)
  case v@Lam(a) => Lam(a.replaceIdxWithNumImpl(offset + 1))(v.binder)
  case v@Sig(a, b) => Sig(a.replaceIdxWithNumImpl(offset), b.replaceIdxWithNumImpl(offset + 1))(v.binder)
  case Pair(a, b) => Pair(a.replaceIdxWithNumImpl(offset), b.replaceIdxWithNumImpl(offset))
  case _ => v
}

private def [T](r: Redux[T])replaceIdxWithNumImpl(offset: Int, tConverter : T => T)(given ctx: Context[?]) : Redux[T] = r match {
  case App(a, b) => App(tConverter(a), b.replaceIdxWithNumImpl(offset + 1))
  case Prj1(p) => Prj1(tConverter(p))
  case Prj2(p) => Prj2(tConverter(p))
  case Global(_) => r
}

private def (r: Reference)replaceIdxWithNumImpl(offset: Int)(given ctx: Context[?]) : Reference = r match {
  case Idx(i) if (i >= offset) => Num(ctx.idxToNum(i, offset))
  case _ => r
}

type ErrorContext = Seq[TypeCheckOps]
