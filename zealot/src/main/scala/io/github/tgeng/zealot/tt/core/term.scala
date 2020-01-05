package io.github.tgeng.zealot.tt.core

trait Derived[+S] {
  def source : Option[S]
}

enum Term[+S] extends Derived[S] {
  case Ref(ref: Reference[S])(val source: Option[S])
  case Val(value: Value[S])(val source: Option[S])
  case Rdx(rdx: Redux[Term[S], S])(val source: Option[S])
}

enum Whnf[+S] extends Derived[S] {
  case Neu(neu: Neutral[S])(val source: Option[S])
  case Val(value: Value[S])(val source: Option[S])
}

enum Neutral[+S] extends Derived[S] {
  case Ref(ref: Reference[S])(val source: Option[S])
  case Rdx(rdx: Redux[Neutral[S], S])(val source: Option[S])
}

enum Reference[+S] extends Derived[S] {
  // De Bruijn index
  case Idx(idx: Int)(val source: Option[S])
  // De Bruijn number (aka context size - 1 - index). Used by type checking
  // logic. Client code should not use this.
  case Num(num: Int)(val source: Option[S])
}

enum Value[+S] extends Derived[S] {
  case Set(level: Int)(val source: Option[S])
  case Pi(dom: Term[S], cod: Term[S])(val source: Option[S])
  case Lam(body: Term[S])(val source: Option[S])
  case Sig(fstTy: Term[S], sndTy: Term[S])(val source: Option[S])
  case Pair(fst: Term[S], snd: Term[S])(val source: Option[S])
  case Unit()(val source: Option[S])
  case Star()(val source: Option[S])
}

enum Redux[+T, +S] extends Derived[S] {
  case App(fn: T, arg: Term[S])(val source: Option[S])
  case Prj1(pair: T)(val source: Option[S])
  case Prj2(pair: T)(val source: Option[S])
}

private def neutralToTerm[S](n: Neutral[S]) : Term[S] = n match {
  case Neutral.Ref(ref) => Term.Ref(ref)(n.source)
  case Neutral.Rdx(rdx) => Term.Rdx(map(neutralToTerm[S])(rdx))(n.source)
}

private def map[F, T, S](mapper: F => T)(r: Redux[F, S]) : Redux[T, S] = r match {
  case Redux.App(fn, arg) => Redux.App(mapper(fn), arg)(r.source)
  case Redux.Prj1(pair) => Redux.Prj1(mapper(pair))(r.source)
  case Redux.Prj2(pair) => Redux.Prj2(mapper(pair))(r.source)
}

def [S](t: Term[S]) raise(amount: Int, bar: Int) =
  t.raised(given RaiseSpec(amount, bar))

// def (t: Whnf) raise(amount: Int, bar: Int) =
//   t.raised(given RaiseSpec(amount, bar))

def [S](t: Term[S]) substituteOutmost(substitute: Term[S]) = t.substitute(0, substitute.raise(1, 0)).raise(-1, 0)


def [S](t: Term[S]) substitute(targetIndex: Int, substitute: Term[S]) =
  t.substituted(given SubstituteSpec(targetIndex: Int, substitute: Term[S]))

private def [S](t: Term[S]) raised(given spec: RaiseSpec) : Term[S] = t match {
  case Term.Ref(r@Reference.Idx(i)) if (i >= spec.bar) => {
    assert (i + spec.amount >= 0)
    Term.Ref(Reference.Idx(i + spec.amount)(r.source))(t.source)
  }
  case Term.Ref(_) => t
  case Term.Val(v) => Term.Val(v.raised)(t.source)
  case Term.Rdx(r) => Term.Rdx(r.raised{ _.raised })(t.source)
}

// private def (nf: Whnf) raised(given spec: RaiseSpec): Whnf = nf match {
//   case Whnf.Neu(n) => Whnf.Neu(n.raised)
//   case Whnf.Val(v) => Whnf.Val(v.raised)
// }

// private def (n: Neutral) raised(given spec: RaiseSpec): Neutral = n match {
//   case Neutral.Var(i) => if (i >= spec.bar) Neutral.Var(i+1) else n
//   case Neutral.Rdx(r) => Neutral.Rdx(r.raised { _.raised })
// }

private def [S](v: Value[S]) raised(given spec: RaiseSpec) : Value[S] = v match {
  case Value.Pi(dom, cod) => Value.Pi(dom.raised, cod.raised(given spec++))(v.source)
  case Value.Lam(body) => Value.Lam(body.raised(given spec++))(v.source)
  case Value.Sig(fstTy, sndTy) => Value.Sig(fstTy.raised, sndTy.raised(given spec++))(v.source)
  case Value.Pair(fst: Term[S], snd: Term[S]) => Value.Pair(fst.raised, snd.raised)(v.source)
  case _ => v
}

private def [T, S](r: Redux[T, S]) raised(tRaiser: T => (given RaiseSpec) => T)(given spec: RaiseSpec) : Redux[T, S] = r match {
  case Redux.App(fn, arg) => Redux.App(tRaiser(fn), arg.raised)(r.source)
  case Redux.Prj1(t) => Redux.Prj1(tRaiser(t))(r.source)
  case Redux.Prj2(t) => Redux.Prj2(tRaiser(t))(r.source)
}

private def [S](t: Term[S]) substituted(given spec: SubstituteSpec[S]) : Term[S] = t match {
  case Term.Ref(Reference.Idx(i)) if (i == spec.targetIndex) => spec.substitute
  case Term.Ref(_) => t
  case Term.Val(v) => Term.Val(v.substituted[S])(t.source)
  case Term.Rdx(r) => Term.Rdx(r.substituted[S])(t.source)
}

private def [S](v: Value[S]) substituted(given spec: SubstituteSpec[S]) : Value[S] = v match {
  case Value.Pi(dom, cod) => Value.Pi(dom.substituted[S], cod.substituted[S](given spec++))(v.source)
  case Value.Lam(body) => Value.Lam(body.substituted[S](given spec++))(v.source)
  case Value.Sig(fstTy, sndTy) => Value.Sig(fstTy.substituted[S], sndTy.substituted[S](given spec++))(v.source)
  case Value.Pair(fst: Term[S], snd: Term[S]) => Value.Pair(fst.substituted, snd.substituted)(v.source)
  case _ => v
}

private def [S](r: Redux[Term[S], S]) substituted(given spec: SubstituteSpec[S]) : Redux[Term[S], S] = r match{
  case Redux.App(fn, arg) => Redux.App(fn.substituted, arg.substituted[S])(r.source)
  case Redux.Prj1(t) => Redux.Prj1(t.substituted)(r.source)
  case Redux.Prj2(t) => Redux.Prj2(t.substituted)(r.source)
}

private case class RaiseSpec(amount: Int, bar: Int) {
  def ++ = RaiseSpec(amount, bar + 1)
}

private case class SubstituteSpec[S](targetIndex: Int, substitute: Term[S]) {
  def ++ = SubstituteSpec(targetIndex + 1, substitute.raised(given RaiseSpec(1, 0)))
}
