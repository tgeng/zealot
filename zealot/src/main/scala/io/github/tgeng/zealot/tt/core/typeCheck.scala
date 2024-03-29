package io.github.tgeng.zealot.tt.core

import scala.math.max
import scala.language.implicitConversions
import scala.collection.immutable.Seq
import io.github.tgeng.zealot.tt.core.Builder.{given, _}
import io.github.tgeng.zealot.tt.core.Neutral._
import io.github.tgeng.zealot.tt.core.Redux._
import io.github.tgeng.zealot.tt.core.Reference._
import io.github.tgeng.zealot.tt.core.Value._
import io.github.tgeng.zealot.tt.core.Whnf._

def (t: Term) checkType(ty: Term)(given ctx: TypeContext)(given glbCtx: GlobalContext) : Either[TypeCheckError, Unit] = try {
  given errCtx : ErrorContext = Seq.empty
  t.whnf.checkType(ty.whnf)(given Seq.empty)
} catch {
  case e: WhnfStuckException => Left(e.toTypeCheckError())
}

def (t: Term) inferType()(given ctx: TypeContext)(given glbCtx: GlobalContext) : Either[TypeCheckError, Type] = try {
  given errCtx : ErrorContext = Seq.empty
  t.whnf.inferType()(given Seq.empty)
} catch {
  case e: WhnfStuckException => Left(e.toTypeCheckError())
}

def (t: Whnf) checkSetType()(given errCtx: ErrorContext)(given ctx: TypeContext): Either[TypeCheckError, Int] = t match {
  case Val(Set(l)) => Right(l)
  case _ => Left(TypeCheckError(s"Expected $t to be a Set at some level.", errCtx, ctx.snapshot))
}

def (t: Whnf) checkTConType(schema: InductiveTypeSchema)(given errCtx: ErrorContext)(given ctx: TypeContext): Either[TypeCheckError, Unit] = t match {
  case Val(TCon(s, content)) if s == schema => Right(())
  case _ => Left(TypeCheckError(s"Expected $t to be inductive type ${schema.name}.", errCtx, ctx.snapshot))
}

private def (t: Whnf) checkType(ty: Type)(given errCtx: ErrorContext)(given ctx: TypeContext)(given glbCtx: GlobalContext) : Either[TypeCheckError, Unit] = {
  given newErrCtx : ErrorContext = errCtx.appended(TypeCheckOps.Check(t, ty))
  (t, ty) match {
    case (Val(Lam(body)), Val(Pi(argTy, bodyTy))) => {
      val argTyWhnf = argTy.whnf
      for {
        argTyTy <- argTyWhnf.inferType()
        _ <- argTyTy.checkSetType()
        result <- (argTy.whnf :: ctx) {
          body.whnf.checkType(bodyTy.whnf)
        }
      } yield result
    }
    case (Val(Pair(a, b)), Val(Sig(aTy, bTy))) => {
      val aTyWhnf = aTy.whnf
      for {
        _ <- a.whnf.checkType(aTyWhnf)
        result <- b.whnf.checkType(bTy.substituteOpen(a).whnf)
      } yield result
    }
    case _ => for {
      tyty <- ty.inferType()
      _ <- tyty.checkSetType()
      inferredType <- t.inferType()
      _ <- inferredType <= ty
    } yield ()
  }
}

private def (t: Whnf) inferType()(given errCtx: ErrorContext)(given ctx: TypeContext)(given glbCtx: GlobalContext) : Either[TypeCheckError, Whnf] = {
  given newErrCtx : ErrorContext = errCtx.appended(TypeCheckOps.Infer(t))
    t match {
    case Neu(v) => v match {
      case Ref(r) => ctx(r) match {
        case Some(ty) => Right(ty)
        case empty => Left(TypeCheckError(s"Unexpected variable at reference $r.", errCtx, ctx.snapshot))
      }
      case Rdx(r) => r match {
        case App(fn, arg) => for {
          fnTy <- Neu(fn).inferType()
          pi <- fnTy.checkPiType()
          (argTy, bodyTy) = pi
          _ <- arg.whnf.checkType(argTy.whnf)
        } yield bodyTy.substituteOpen(arg).whnf
        case Prj1(pair) => for {
          pairTy <- Neu(pair).inferType()
          sig <- pairTy.checkSigType()
          (aTy, _) = sig
        } yield aTy.whnf
        case Prj2(pair) => for {
          pairTy <- Neu(pair).inferType()
          sig <- pairTy.checkSigType()
          (aTy, bTy) = sig
        } yield bTy.substituteOpen(Term.Rdx(Prj1(Neu(pair).term))).whnf
        case Global(qn) => glbCtx.getType(qn) match {
          case Some(ty) => Right(ty.whnf)
          case _ => Left(TypeCheckError(s"Could not find global reference $qn", errCtx, ctx.snapshot))
        }
      }
    }
    case Val(v) => v match {
        case Set(i) => Right(Val(Set(i+1)))
        case Pi(argTy, bodyTy) => {
          val argTyWhnf = argTy.whnf
          for {
            argTyTy <- argTyWhnf.inferType()
            argLevel <- argTyTy.checkSetType()
            bodyLevel <- (argTyWhnf :: ctx) {
              for {
                bodyTyTy <- bodyTy.whnf.inferType()
                bodyLevel <- bodyTyTy.checkSetType()
              } yield bodyLevel
            }
          } yield Val(Set(max(argLevel, bodyLevel)))
        }
        case Lam(_) => Left(TypeCheckError("Cannot infer type of a lambda.", errCtx, ctx.snapshot))
        case Sig(aTy, bTy) => {
          val aTyWhnf = aTy.whnf
          for {
            aTyTy <- aTyWhnf.inferType()
            aLevel <- aTyTy.checkSetType()
            bLevel <- (aTyWhnf :: ctx) {
              for {
                bTyTy <- bTy.whnf.inferType()
                bLevel <- bTyTy.checkSetType()
              } yield bLevel
            }
          } yield Val(Set(max(aLevel, bLevel)))
        }
        case Pair(_, _) => Left(TypeCheckError("Cannot infer type of a pair.", errCtx, ctx.snapshot))
        case Unit => Right(Val(Set(0)))
        case Star => Right(Val(Unit))
        case TCon(schema, content) => Right(substituteContent(schema.targetType, content))
        case VCon(schema, content) => Right(substituteContent(schema.targetType, content))
    }
  }
}

private def substituteContent(targetType: Term, content: Seq[Term])(given errCtx: ErrorContext)(given glbCtx: GlobalContext): Whnf = {
  val (argTypes, bodyType) = flattenPi(targetType)
  assert(argTypes.size == content.size)
  val substitutedBodyType = content.foldRight[Term](bodyType)((arg, bodyType) => bodyType.substituteOpen(arg))
  substitutedBodyType.whnf
}

private def (a: Whnf) <= (b: Whnf)(given errCtx: ErrorContext)(given ctx: TypeContext)(given glbCtx: GlobalContext) : Either[TypeCheckError, Unit] = {
  given newErrCtx : ErrorContext = errCtx.appended(TypeCheckOps.Subtype(a, b))
  def raiseError() = Left(TypeCheckError(s"$a is not a subtype of $b.", errCtx, ctx.snapshot))
  (a, b) match {
    case (Val(Set(iA)), Val(Set(iB))) => if (iA <= iB) Right(()) else raiseError()
    case (Val(Pi(argTyA, bodyTyA)), Val(Pi(argTyB, bodyTyB))) => for {
      _ <- (argTyA.whnf ~= argTyB.whnf)(Val(Set(-1)))
      _ <- (argTyA.whnf :: ctx) {
        bodyTyA.whnf <= bodyTyB.whnf
      }
    } yield ()
    case (Val(Sig(aTyA, bTyA)), Val(Sig(aTyB, bTyB))) => for {
      _ <- (aTyA.whnf ~= aTyB.whnf)(Val(Set(-1)))
      _ <- (aTyA.whnf :: ctx) {
        bTyA.whnf <= bTyB.whnf
      }
    } yield ()
    case (_, _) => (a ~= b)(Val(Set(-1)))
  }
}

// TODO(tgeng): consider doing this coinductively
private def (a: Whnf) ~= (b: Whnf)(ty: Type)(given errCtx: ErrorContext)(given ctx: TypeContext)(given glbCtx: GlobalContext) : Either[TypeCheckError, Unit] = {
  given newErrCtx : ErrorContext = errCtx.appended(TypeCheckOps.TermConvertible(a, b, ty))
  def raiseError() = Left(TypeCheckError(s"Term $a and $b are not convertible.", errCtx, ctx.snapshot))
  if (a == b) return Right(())
  (ty, a, b) match {
    case (Val(Set(_)), Val(Set(lA)), Val(Set(lB))) =>
      if (lA == lB) Right(())
      else raiseError()
    case (Val(Set(_)), Val(Pi(argTyA, bodyTyA)), Val(Pi(argTyB, bodyTyB))) => {
      val argTyAWhnf = argTyA.whnf
      for {
        _ <- (argTyAWhnf ~= argTyB.whnf)(ty)
        _ <- (argTyAWhnf :: ctx) {
          (bodyTyA.whnf ~= bodyTyB.whnf)(ty)
        }
      } yield ()
    }
    case (Val(Set(_)), Val(Sig(aTyA, bTyA)), Val(Sig(aTyB, bTyB))) => {
      val aTyAWhnf = aTyA.whnf
      for {
        _ <- (aTyAWhnf ~= aTyB.whnf)(ty)
        _ <- (aTyAWhnf :: ctx) {
          (bTyA.whnf ~= bTyB.whnf)(ty)
        }
      } yield ()
    }
    case (Val(Unit), _, _) => Right(())
    case (Val(Pi(argTy, bodyTy)), _, _) => (argTy.whnf :: ctx) {
      ((a.term)(0.ref).whnf ~= (b.term)(0.ref).whnf)(bodyTy.whnf)
    }
    case (Val(Sig(aTy, bTy)), _, _) => for {
      _ <- (p1(a.term).whnf ~= p1(b.term).whnf)(aTy.whnf)
      _ <- (p2(a.term).whnf ~= p2(b.term).whnf)(bTy.substituteOpen(a.term).whnf)
    } yield ()
    case (_, Neu(nA), Neu(nB)) => (nA === nB).map { _ => ()}
    case _ => raiseError()
  }
}

// TODO(tgeng): consider doing this co-inductively
private def (a: Neutral) === (b: Neutral)(given errCtx: ErrorContext)(given ctx: TypeContext)(given glbCtx: GlobalContext) : Either[TypeCheckError, Type] = {
  given newErrCtx : ErrorContext = errCtx.appended(TypeCheckOps.NeutralConvertible(a, b))
  def raiseError() = Left(TypeCheckError(s"Neutral $a and $b are not convertible.", errCtx, ctx.snapshot))
  (a, b) match {
    case (Ref(aR), Ref(bR)) =>
      if (aR == bR) {
        Neu(a).inferType()
      } else {
        (aR, bR) match {
          case (Idx(i), Num(n)) => if (ctx.isIdxEqualNum(i, n)) Neu(a).inferType() else raiseError()
          case (Num(n), Idx(i)) => if (ctx.isIdxEqualNum(i, n)) Neu(a).inferType() else raiseError()
          case _ => raiseError()
        }
      }
    case (Rdx(App(aFn, aArg)), Rdx(App(bFn, bArg))) => for {
      fnTy <- aFn === bFn
      pi <- fnTy.checkPiType()
      (argTy, bodyTy) = pi
      _ <- (aArg.whnf ~= bArg.whnf)(argTy.whnf)
    } yield bodyTy.substituteOpen(aArg).whnf
    case (Rdx(Prj1(pairA)), Rdx(Prj2(pairB))) => for {
      pairTy <- pairA === pairB
      sig <- pairTy.checkSigType()
      (aTy, _) = sig
    } yield aTy.whnf
    case (Rdx(Prj2(pairA)), Rdx(Prj2(pairB))) => for {
      pairTy <- pairA === pairB
      sig <- pairTy.checkSigType()
      (aTy, bTy) = sig
    } yield bTy.substituteOpen(Term.Rdx(Prj1(Neu(pairA).term))).whnf
    case _ => raiseError()
  }
}

class TypeCheckError(val message: String, val errorContext: ErrorContext, val ctx: Seq[Type]) extends Exception(message)

enum TypeCheckOps {
  case Check(t: Whnf, ty: Whnf)
  case Infer(t: Whnf)
  case Subtype(a: Whnf, b: Whnf)
  case TermConvertible(a: Whnf, b: Whnf, ty: Whnf)
  case NeutralConvertible(a: Neutral, b: Neutral)
}

private def (e: WhnfStuckException) toTypeCheckError() = {
  var msg = e.getMessage()
  if (msg == null) msg = ""
  TypeCheckError(msg, e.errorContext, Seq.empty)
}

private def (t: Whnf) checkPiType()(given errCtx: ErrorContext)(given ctx: TypeContext): Either[TypeCheckError, (Term, Term)] = t match {
  case Val(Pi(argTy, bodyTy)) => Right(argTy, bodyTy)
  case _ => Left(TypeCheckError(s"Expected $t to be a dependent product type", errCtx, ctx.snapshot))
}

private def (t: Whnf) checkSigType()(given errCtx: ErrorContext)(given ctx: TypeContext): Either[TypeCheckError, (Term, Term)] = t match {
  case Val(Sig(aTy, bTy)) => Right(aTy, bTy)
  case _ => Left(TypeCheckError(s"Expected $t to be a dependent pair type.", errCtx, ctx.snapshot))
}
