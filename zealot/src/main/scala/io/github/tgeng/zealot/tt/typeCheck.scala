package io.github.tgeng.zealot.tt

import scala.math.max
import scala.language.implicitConversions
import scala.collection.immutable.Seq
import io.github.tgeng.zealot.tt.Builder.{given, _}
import io.github.tgeng.zealot.tt.Neutral._
import io.github.tgeng.zealot.tt.Redux._
import io.github.tgeng.zealot.tt.Reference._
import io.github.tgeng.zealot.tt.Value._
import io.github.tgeng.zealot.tt.Whnf._

def (t: Term) checkType(ty: Term)(given ctx: Context) : Either[TypeCheckError, Unit] = try {
  given errCtx : ErrorContext = Seq.empty
  t.whnf.checkType(ty.whnf)(given Seq.empty)
} catch {
  case e: WhnfStuckException => Left(e.toTypeCheckError())
}

def (t: Term) inferType()(given ctx: Context) : Either[TypeCheckError, Type] = try {
  given errCtx : ErrorContext = Seq.empty
  t.whnf.inferType()(given Seq.empty)
} catch {
  case e: WhnfStuckException => Left(e.toTypeCheckError())
}

private def (t: Whnf) checkType(ty: Type)(given errCtx: ErrorContext)(given ctx: Context) : Either[TypeCheckError, Unit] = {
  given newErrCtx : ErrorContext = errCtx.appended(TypeCheckOps.Check(t, ty))
  (t, ty) match {
    case (Val(Lam(body)), Val(Pi(argTy, bodyTy))) => {
      (ctx :: argTy.whnf) { () => 
        body.whnf.checkType(bodyTy.whnf) 
      }
    }
    case (Val(Pair(a, b)), Val(Sig(aTy, bTy))) => {
      val aTyWhnf = aTy.whnf
      for {
        _ <- a.whnf.checkType(aTyWhnf)
        result <- b.whnf.checkType(bTy.substituteOutmost(a).whnf)
      } yield result
    }
    case _ => for {
      inferredType <- t.inferType()
      _ <- inferredType <= ty
    } yield ()
  }
}

private def (t: Whnf) inferType()(given errCtx: ErrorContext)(given ctx: Context) : Either[TypeCheckError, Whnf] = {
  given newErrCtx : ErrorContext = errCtx.appended(TypeCheckOps.Infer(t))
    t match {
    case Neu(v) => v match {
      case Ref(r) => ctx(r) match {
        case Some(ty) => Right(ty)
        case empty => Left(TypeCheckError(s"Unexpected variable at reference $r", errCtx))
      }
      case Rdx(r) => r match {
        case App(fn, arg) => for {
          fnTy <- Neu(fn).inferType()
          pi <- fnTy.checkPiType()
          (argTy, bodyTy) = pi
          _ <- arg.whnf.checkType(argTy.whnf)
        } yield bodyTy.substituteOutmost(arg).whnf
        case Prj1(pair) => for {
          pairTy <- Neu(pair).inferType()
          sig <- pairTy.checkSigType()
          (aTy, _) = sig
        } yield aTy.whnf
        case Prj2(pair) => for {
          pairTy <- Neu(pair).inferType()
          sig <- pairTy.checkSigType()
          (aTy, bTy) = sig
        } yield bTy.substituteOutmost(Term.Rdx(Prj1(Neu(pair).term))).whnf
      }
    }
    case Val(v) => v match {
        case Set(i) => Right(Val(Set(i+1)))
        case Pi(argTy, bodyTy) => for {
          argTyTy <- argTy.whnf.inferType()
          argLevel <- argTyTy.checkSetType()
          bodyTyTy <- bodyTy.whnf.inferType()
          bodyLevel <- bodyTyTy.checkSetType()
        } yield Val(Set(max(argLevel, bodyLevel)))
        case Lam(_) => Left(TypeCheckError("cannot infer type of lam", errCtx))
        case Sig(aTy, bTy) => for {
          aTyTy <- aTy.whnf.inferType()
          aLevel <- aTyTy.checkSetType()
          bTyTy <- bTy.whnf.inferType()
          bLevel <- bTyTy.checkSetType()
        } yield Val(Set(max(aLevel, bLevel)))
        case Pair(_, _) => Left(TypeCheckError("cannot infer type of pair", errCtx))
        case Unit => Right(Val(Set(0)))
        case Star => Right(Val(Unit))
    }
  }
}

private def (a: Whnf) <= (b: Whnf)(given errCtx: ErrorContext)(given ctx: Context) : Either[TypeCheckError, Unit] = {
  given newErrCtx : ErrorContext = errCtx.appended(TypeCheckOps.Subtype(a, b))
  def raiseError() = Left(TypeCheckError(s"$a is not a subtype of $b", errCtx))
  (a, b) match {
    case (Val(Set(iA)), Val(Set(iB))) => if (iA <= iB) Right(()) else raiseError()
    case (Val(Pi(argTyA, bodyTyA)), Val(Pi(argTyB, bodyTyB))) => for {
      _ <- (argTyA.whnf ~= argTyB.whnf)(Val(Set(-1)))
      _ <- (ctx :: argTyA.whnf) { () =>
        bodyTyA.whnf <= bodyTyB.whnf
      }
    } yield ()
    case (Val(Sig(aTyA, bTyA)), Val(Sig(aTyB, bTyB))) => for {
      _ <- (aTyA.whnf ~= aTyB.whnf)(Val(Set(-1)))
      _ <- (ctx :: aTyA.whnf) { () =>
        bTyA.whnf <= bTyB.whnf
      }
    } yield ()
    case (_, _) => (a ~= b)(Val(Set(-1)))
  }
}

private def (a: Whnf) ~= (b: Whnf)(ty: Type)(given errCtx: ErrorContext)(given ctx: Context) : Either[TypeCheckError, Unit] = {
  given newErrCtx : ErrorContext = errCtx.appended(TypeCheckOps.TermConvertible(a, b, ty))
  def raiseError() = Left(TypeCheckError(s"$a and $b are not convertible", errCtx))
  if (a == b) return Right(())
  (ty, a, b) match {
    case (Val(Set(_)), Val(Set(lA)), Val(Set(lB))) => 
      if (lA == lB) Right(()) 
      else raiseError()
    case (Val(Set(_)), Val(Pi(argTyA, bodyTyA)), Val(Pi(argTyB, bodyTyB))) => {
      val argTyAWhnf = argTyA.whnf
      for {
        _ <- (argTyAWhnf ~= argTyB.whnf)(ty)
        _ <- (ctx :: argTyAWhnf) { () =>
          (bodyTyA.whnf ~= bodyTyB.whnf)(ty)
        }
      } yield ()
    }
    case (Val(Set(_)), Val(Sig(aTyA, bTyA)), Val(Sig(aTyB, bTyB))) => {
      val aTyAWhnf = aTyA.whnf
      for {
        _ <- (aTyAWhnf ~= aTyB.whnf)(ty)
        _ <- (ctx :: aTyAWhnf) { () =>
          (bTyA.whnf ~= bTyB.whnf)(ty)
        }
      } yield ()
    }
    case (Val(Unit), _, _) => Right(())
    case (Val(Pi(argTy, bodyTy)), _, _) => (ctx :: argTy.whnf) { () =>
      ((a.term)(!0).whnf ~= (b.term)(!0).whnf)(bodyTy.whnf)
    }
    case (Val(Sig(aTy, bTy)), _, _) => for {
      _ <- (p1(a.term).whnf ~= p1(b.term).whnf)(aTy.whnf)
      _ <- (p2(a.term).whnf ~= p2(b.term).whnf)(bTy.substituteOutmost(a.term).whnf)
    } yield ()
    case (_, Neu(nA), Neu(nB)) => (nA === nB).map { _ => ()}
    case _ => raiseError()
  }
}

private def (a: Neutral) === (b: Neutral)(given errCtx: ErrorContext)(given ctx: Context) : Either[TypeCheckError, Type] = {
  given newErrCtx : ErrorContext = errCtx.appended(TypeCheckOps.NeutralConvertible(a, b))
  def raiseError() = Left(TypeCheckError(s"neutral $a and $b are not convertible", errCtx))
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
    } yield bodyTy.substituteOutmost(aArg).whnf
    case (Rdx(Prj1(pairA)), Rdx(Prj2(pairB))) => for {
      pairTy <- pairA === pairB
      sig <- pairTy.checkSigType()
      (aTy, _) = sig
    } yield aTy.whnf
    case (Rdx(Prj2(pairA)), Rdx(Prj2(pairB))) => for {
      pairTy <- pairA === pairB
      sig <- pairTy.checkSigType()
      (aTy, bTy) = sig
    } yield bTy.substituteOutmost(Term.Rdx(Prj1(Neu(pairA).term))).whnf
    case _ => raiseError()
  }
}

class TypeCheckError(val message: String, val errorContext: ErrorContext) extends Exception(message)

enum TypeCheckOps {
  case Check(t: Whnf, ty: Whnf)
  case Infer(t: Whnf)
  case Subtype(a: Whnf, b: Whnf)
  case TermConvertible(a: Whnf, b: Whnf, ty: Whnf)
  case NeutralConvertible(a: Neutral, b: Neutral)
}

private def (e: WhnfStuckException) toTypeCheckError() = TypeCheckError(e.getMessage, e.errorContext)

private def (t: Whnf) checkPiType()(given errCtx: ErrorContext): Either[TypeCheckError, (Term, Term)] = t match {
  case Val(Pi(argTy, bodyTy)) => Right(argTy, bodyTy)
  case _ => Left(TypeCheckError(s"Expected $t to be a dependent product type", errCtx))
}

private def (t: Whnf) checkSigType()(given errCtx: ErrorContext): Either[TypeCheckError, (Term, Term)] = t match {
  case Val(Sig(aTy, bTy)) => Right(aTy, bTy)
  case _ => Left(TypeCheckError(s"Expected $t to be a dependent pair type.", errCtx))
}

private def (t: Whnf) checkSetType()(given errCtx: ErrorContext): Either[TypeCheckError, Int] = t match {
  case Val(Set(l)) => Right(l)
  case _ => Left(TypeCheckError(s"Expected $t to be a Set at some level.", errCtx))
}
