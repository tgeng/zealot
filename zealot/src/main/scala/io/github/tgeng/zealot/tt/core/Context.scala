package io.github.tgeng.zealot.tt.core

import scala.collection.mutable.ArrayBuffer
import io.github.tgeng.zealot.tt.core.Neutral._
import io.github.tgeng.zealot.tt.core.Reference._
import io.github.tgeng.zealot.tt.core.Redux._
import io.github.tgeng.zealot.tt.core.Term._
import io.github.tgeng.zealot.tt.core.Whnf._
import io.github.tgeng.zealot.tt.core.Value._

type Type = Whnf

class Context[S]() {
  val content: ArrayBuffer[Type[S]] = ArrayBuffer()
  def apply(r: Reference[S]) : Option[Type[S]] = r match {
    case Idx(i) => if (i < 0 || i >= content.size) Option.empty else Option(content(content.size - i - 1))
    case Num(n) => if (n < 0 || n >= content.size) Option.empty else Option(content(n))
  }

  def +=(tys: Type[S]*) = for (ty <- tys) content.append(ty.replaceIdxWithNum(0)(given this))

  def ++=(tys: Traversable[Type[S]]) = this.+=(tys.toSeq : _*)

  def ::[T](ty: Type[S])(action: => T) : T = {
    +=(ty)
    val result = action
    content.dropRightInPlace(1)
    result
  }

  def isIdxEqualNum(idx: Int, num: Int) = idx + num + 1 == content.size
  def idxToNum(idx: Int, offset: Int) : Int = content.size + offset - 1 - idx
  def snapshot : Seq[Type[S]] = content.toList

}

private def [S](w: Whnf[S])replaceIdxWithNum(offset: Int)(given ctx: Context[S]) : Whnf[S] = w match {
  case Neu(n) => Neu(n.replaceIdxWithNum[S](offset))(w.source)
  case Whnf.Val(v) => Whnf.Val(v.replaceIdxWithNum[S](offset))(w.source)
}

private def [S](t: Term[S])replaceIdxWithNum(offset: Int)(given ctx: Context[S]) : Term[S] = t match {
  case Term.Ref(r) => Term.Ref(r.replaceIdxWithNum[S](offset))(t.source)
  case Term.Val(v) => Term.Val(v.replaceIdxWithNum[S](offset))(t.source)
  case Term.Rdx(r) => Term.Rdx(r.replaceIdxWithNum[Term[S], S](offset, t => t.replaceIdxWithNum[S](offset)))(t.source)
}

private def [S](n: Neutral[S])replaceIdxWithNum(offset: Int)(given ctx: Context[S]) : Neutral[S] = n match {
  case Neutral.Ref(r) => Neutral.Ref(r.replaceIdxWithNum[S](offset))(n.source)
  case Neutral.Rdx(r) => Neutral.Rdx(r.replaceIdxWithNum[Neutral[S], S](offset, t => t.replaceIdxWithNum[S](offset)))(n.source)
}

private def [S](v: Value[S])replaceIdxWithNum(offset: Int)(given ctx: Context[S]) : Value[S] = v match {
  case Pi(a, b) => Pi(a.replaceIdxWithNum[S](offset), b.replaceIdxWithNum[S](offset + 1))(v.source)
  case Lam(a) => Lam(a.replaceIdxWithNum[S](offset + 1))(v.source)
  case Sig(a, b) => Sig(a.replaceIdxWithNum[S](offset), b.replaceIdxWithNum[S](offset + 1))(v.source)
  case Pair(a, b) => Pair(a.replaceIdxWithNum[S](offset), b.replaceIdxWithNum[S](offset))(v.source)
  case _ => v
}

private def [T, S](r: Redux[T, S])replaceIdxWithNum(offset: Int, tConverter : T => T)(given ctx: Context[S]) : Redux[T, S] = r match {
  case App(a, b) => App(tConverter(a), b.replaceIdxWithNum[S](offset + 1))(r.source)
  case Prj1(p) => Prj1(tConverter(p))(r.source)
  case Prj2(p) => Prj2(tConverter(p))(r.source)
}

private def [S](r: Reference[S])replaceIdxWithNum(offset: Int)(given ctx: Context[S]) : Reference[S] = r match {
  case Idx(i) if (i >= offset) => Num(ctx.idxToNum(i, offset))(r.source)
  case _ => r
}

type ErrorContext[S] = Seq[TypeCheckOps[S]]
