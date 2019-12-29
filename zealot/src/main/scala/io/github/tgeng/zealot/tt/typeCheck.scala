package io.github.tgeng.zealot.tt

import scala.math.max
import io.github.tgeng.zealot.tt.Neutral._
import io.github.tgeng.zealot.tt.Redux._
import io.github.tgeng.zealot.tt.Value._
import io.github.tgeng.zealot.tt.Whnf._

def (t: Term) checkType(ty: Term) : Either[TypeCheckError, Unit] = {
  throw UnsupportedOperationException()
}

private def (t: Whnf) checkType(ty: Type)(given ctx: Context) : Either[TypeCheckError, Unit] = (t, ty) match {
  case (Val(Lam(body)), Val(Pi(argTy, bodyTy))) => {
    (ctx :: argTy.whnf) { () => 
      body.whnf.checkType(bodyTy.whnf) 
    }
  }
  case (Val(Pair(a, b)), Val(Sig(aTy, bTy))) => {
    val aTyWhnf = aTy.whnf
    for {
      _ <- a.whnf.checkType(aTyWhnf)
      result <- b.whnf.checkType(bTy.substituteOutmost(aTyWhnf.term).whnf)
    } yield result
  }
  case _ => for {
    inferredType <- t.inferType()
    _ <- inferredType <= ty
  } yield ()
}

private def (t: Whnf) inferType()(given ctx: Context) : Either[TypeCheckError, Whnf] = t match {
  case Neu(v) => v match {
    case Var(i) => ctx(i) match {
      case Some(ty) => Right(ty)
      case empty => Left(TypeCheckError(s"Unexpected variable index $i"))
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
      } yield bTy.substituteOutmost(aTy).whnf
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
      case Lam => Left(TypeCheckError("cannot infer type of lam"))
      case Sig(aTy, bTy) => for {
        aTyTy <- aTy.whnf.inferType()
        aLevel <- aTyTy.checkSetType()
        bTyTy <- bTy.whnf.inferType()
        bLevel <- bTyTy.checkSetType()
      } yield Val(Set(max(aLevel, bLevel)))
      case Pair => Left(TypeCheckError("cannot infer type of pair"))
      case Unit => Right(Val(Set(0)))
      case Star => Right(Val(Unit))
  }
}

private def (t: Whnf) checkPiType(): Either[TypeCheckError, (Term, Term)] = t match {
  case Val(Pi(argTy, bodyTy)) => Right(argTy, bodyTy)
  case _ => Left(TypeCheckError(s"Expected $t to be a dependent product type"))
}

private def (t: Whnf) checkSigType(): Either[TypeCheckError, (Term, Term)] = t match {
  case Val(Sig(aTy, bTy)) => Right(aTy, bTy)
  case _ => Left(TypeCheckError(s"Expected $t to be a dependent pair type."))
}

private def (t: Whnf) checkSetType(): Either[TypeCheckError, Int] = t match {
  case Val(Set(l)) => Right(l)
  case _ => Left(TypeCheckError(s"Expected $t to be a Set at some level."))
}

def (a: Whnf) <= (b: Whnf) (given ctx: Context) : Either[TypeCheckError, Unit] = {
  throw UnsupportedOperationException()
}

class TypeCheckError(message: String)

private def (e: WhnfStuckException) toTypeCheckError() = TypeCheckError(e.getMessage)