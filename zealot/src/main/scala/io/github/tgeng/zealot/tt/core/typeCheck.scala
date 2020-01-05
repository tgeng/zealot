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

def [S](t: Term[S]) checkType(ty: Term[S])(given ctx: Context[S]) : Either[TypeCheckError[S], scala.Unit] = try {
  given errCtx : ErrorContext[S] = Seq.empty
  t.whnf.checkType(ty.whnf)(given Seq.empty)
} catch {
  case e: WhnfStuckException[S] => Left(e.toTypeCheckError())
}

def [S](t: Term[S]) inferType()(given ctx: Context[S]) : Either[TypeCheckError[S], Type[S]] = try {
  given errCtx : ErrorContext[S] = Seq.empty
  t.whnf.inferType()(given Seq.empty)
} catch {
  case e: WhnfStuckException[S] => Left(e.toTypeCheckError())
}

private def [S](t: Whnf[S]) checkType(ty: Type[S])(given errCtx: ErrorContext[S])(given ctx: Context[S]) : Either[TypeCheckError[S], scala.Unit] = {
  given newErrCtx : ErrorContext[S] = errCtx.appended(TypeCheckOps.Check(t, ty))
  (t, ty) match {
    case (Val(Lam(body)), Val(Pi(argTy, bodyTy))) => {
      val argTyWhnf = argTy.whnf[S]
      for {
        argTyTy <- argTyWhnf.inferType[S]()
        _ <- argTyTy.checkSetType()
        result <- (argTy.whnf[S] :: ctx) {
          body.whnf[S].checkType(bodyTy.whnf[S])
        }
      } yield result
    }
    case (Val(Pair(a, b)), Val(Sig(aTy, bTy))) => {
      val aTyWhnf = aTy.whnf[S]
      for {
        _ <- a.whnf[S].checkType(aTyWhnf)
        result <- b.whnf[S].checkType(bTy.substituteOutmost(a).whnf)
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

private def [S](t: Whnf[S]) inferType()(given errCtx: ErrorContext[S])(given ctx: Context[S]) : Either[TypeCheckError[S], Whnf[S]] = {
  given newErrCtx : ErrorContext[S] = errCtx.appended(TypeCheckOps.Infer(t))
    t match {
    case Neu(v) => v match {
      case Ref(r) => ctx(r) match {
        case Some(ty) => Right(ty)
        case empty => Left(TypeCheckError(s"Unexpected variable at reference $r.", errCtx, ctx.snapshot))
      }
      case Rdx(r) => r match {
        case App(fn, arg) => for {
          fnTy <- Neu(fn)(None).inferType[S]()
          pi <- fnTy.checkPiType()
          (argTy, bodyTy) = pi
          _ <- arg.whnf[S].checkType(argTy.whnf)
        } yield bodyTy.substituteOutmost(arg).whnf
        case Prj1(pair) => for {
          pairTy <- Neu(pair)(None).inferType[S]()
          sig <- pairTy.checkSigType()
          (aTy, _) = sig
        } yield aTy.whnf
        case Prj2(pair) => for {
          pairTy <- Neu(pair)(None).inferType[S]()
          sig <- pairTy.checkSigType()
          (aTy, bTy) = sig
        } yield bTy.substituteOutmost[S](Term.Rdx(Prj1(Neu(pair)(None).term)(None))(None)).whnf[S]
      }
    }
    case Val(v) => v match {
        case Set(i) => Right(Val(Set(i+1)(None))(None))
        case Pi(argTy, bodyTy) => {
          val argTyWhnf = argTy.whnf[S]
          for {
            argTyTy <- argTyWhnf.inferType[S]()
            argLevel <- argTyTy.checkSetType()
            bodyLevel <- (argTyWhnf :: ctx) {
              for {
                bodyTyTy <- bodyTy.whnf[S].inferType[S]()
                bodyLevel <- bodyTyTy.checkSetType()
              } yield bodyLevel
            }
          } yield Val(Set(max(argLevel, bodyLevel))(None))(None)
        }
        case Lam(_) => Left(TypeCheckError("Cannot infer type of a lambda.", errCtx, ctx.snapshot))
        case Sig(aTy, bTy) => {
          val aTyWhnf = aTy.whnf[S]
          for {
            aTyTy <- aTyWhnf.inferType[S]()
            aLevel <- aTyTy.checkSetType()
            bLevel <- (aTyWhnf :: ctx) {
              for {
                bTyTy <- bTy.whnf[S].inferType[S]()
                bLevel <- bTyTy.checkSetType()
              } yield bLevel
            }
          } yield Val(Set(max(aLevel, bLevel))(None))(None)
        }
        case Pair(_, _) => Left(TypeCheckError("Cannot infer type of a pair.", errCtx, ctx.snapshot))
        case Unit => Right(Val(Set(0)(None))(None))
        case Star => Right(Val(Unit()(None))(None))
    }
  }
}

private def [S](a: Whnf[S]) <= (b: Whnf[S])(given errCtx: ErrorContext[S])(given ctx: Context[S]) : Either[TypeCheckError[S], scala.Unit] = {
  given newErrCtx : ErrorContext[S] = errCtx.appended(TypeCheckOps.Subtype(a, b))
  def raiseError() = Left(TypeCheckError(s"$a is not a subtype of $b.", errCtx, ctx.snapshot))
  (a, b) match {
    case (Val(Set(iA)), Val(Set(iB))) => if (iA <= iB) Right(()) else raiseError()
    case (Val(Pi(argTyA, bodyTyA)), Val(Pi(argTyB, bodyTyB))) => for {
      _ <- (argTyA.whnf[S] ~= argTyB.whnf[S])(Val(Set(-1)(None))(None))
      _ <- (argTyA.whnf[S] :: ctx) {
        bodyTyA.whnf[S] <= bodyTyB.whnf[S]
      }
    } yield ()
    case (Val(Sig(aTyA, bTyA)), Val(Sig(aTyB, bTyB))) => for {
      _ <- (aTyA.whnf[S] ~= aTyB.whnf[S])(Val(Set(-1)(None))(None))
      _ <- (aTyA.whnf[S] :: ctx) {
        bTyA.whnf[S] <= bTyB.whnf[S]
      }
    } yield ()
    case (_, _) => (a ~= b)(Val(Set(-1)(None))(None))
  }
}

// TODO(tgeng): consider doing this coinductively
private def [S](a: Whnf[S]) ~= (b: Whnf[S])(ty: Type[S])(given errCtx: ErrorContext[S])(given ctx: Context[S]) : Either[TypeCheckError[S], scala.Unit] = {
  given newErrCtx : ErrorContext[S] = errCtx.appended(TypeCheckOps.TermConvertible(a, b, ty))
  def raiseError() = Left(TypeCheckError(s"Term $a and $b are not convertible.", errCtx, ctx.snapshot))
  if (a == b) return Right(())
  (ty, a, b) match {
    case (Val(Set(_)), Val(Set(lA)), Val(Set(lB))) =>
      if (lA == lB) Right(())
      else raiseError()
    case (Val(Set(_)), Val(Pi(argTyA, bodyTyA)), Val(Pi(argTyB, bodyTyB))) => {
      val argTyAWhnf = argTyA.whnf[S]
      for {
        _ <- (argTyAWhnf ~= argTyB.whnf[S])(ty)
        _ <- (argTyAWhnf :: ctx) {
          (bodyTyA.whnf[S] ~= bodyTyB.whnf[S])(ty)
        }
      } yield ()
    }
    case (Val(Set(_)), Val(Sig(aTyA, bTyA)), Val(Sig(aTyB, bTyB))) => {
      val aTyAWhnf = aTyA.whnf[S]
      for {
        _ <- (aTyAWhnf ~= aTyB.whnf[S])(ty)
        _ <- (aTyAWhnf :: ctx) {
          (bTyA.whnf[S] ~= bTyB.whnf[S])(ty)
        }
      } yield ()
    }
    case (Val(Unit), _, _) => Right(())
    case (Val(Pi(argTy, bodyTy)), _, _) => (argTy.whnf[S] :: ctx) {
      ((a.term)(!0).whnf ~= (b.term)(!0).whnf)(bodyTy.whnf[S])
    }
    case (Val(Sig(aTy, bTy)), _, _) => for {
      _ <- (p1(a.term).whnf ~= p1(b.term).whnf)(aTy.whnf[S])
      _ <- (p2(a.term).whnf ~= p2(b.term).whnf)(bTy.substituteOutmost(a.term).whnf)
    } yield ()
    case (_, Neu(nA), Neu(nB)) => (nA === nB).map { _ => ()}
    case _ => raiseError()
  }
}

// TODO(tgeng): consider doing this co-inductively
private def [S](a: Neutral[S]) === (b: Neutral[S])(given errCtx: ErrorContext[S])(given ctx: Context[S]) : Either[TypeCheckError[S], Type[S]] = {
  given newErrCtx : ErrorContext[S] = errCtx.appended(TypeCheckOps.NeutralConvertible(a, b))
  def raiseError() = Left(TypeCheckError(s"Neutral $a and $b are not convertible.", errCtx, ctx.snapshot))
  (a, b) match {
    case (Ref(aR), Ref(bR)) =>
      if (aR == bR) {
        Neu(a)(None).inferType()
      } else {
        (aR, bR) match {
          case (Idx(i), Num(n)) => if (ctx.isIdxEqualNum(i, n)) Neu(a)(None).inferType() else raiseError()
          case (Num(n), Idx(i)) => if (ctx.isIdxEqualNum(i, n)) Neu(a)(None).inferType() else raiseError()
          case _ => raiseError()
        }
      }
    case (Rdx(App(aFn, aArg)), Rdx(App(bFn, bArg))) => for {
      fnTy <- aFn === bFn
      pi <- fnTy.checkPiType()
      (argTy, bodyTy) = pi
      _ <- (aArg.whnf[S] ~= bArg.whnf[S])(argTy.whnf)
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
    } yield bTy.substituteOutmost(Term.Rdx(Prj1(Neu(pairA)(None).term)(None))(None)).whnf[S]
    case _ => raiseError()
  }
}

class TypeCheckError[+S](val message: String, val errorContext: ErrorContext[S], val ctx: Seq[Type[S]]) extends Exception(message)

enum TypeCheckOps[+S] {
  case Check(t: Whnf[S], ty: Whnf[S])
  case Infer(t: Whnf[S])
  case Subtype(a: Whnf[S], b: Whnf[S])
  case TermConvertible(a: Whnf[S], b: Whnf[S], ty: Whnf[S])
  case NeutralConvertible(a: Neutral[S], b: Neutral[S])
}

private def [S](e: WhnfStuckException[S]) toTypeCheckError() = {
  var msg = e.getMessage()
  if (msg == null) msg = ""
  TypeCheckError[S](msg, e.errorContext, Seq.empty)
}

private def [S](t: Whnf[S]) checkPiType()(given errCtx: ErrorContext[S])(given ctx: Context[S]): Either[TypeCheckError[S], (Term[S], Term[S])] = t match {
  case Val(Pi(argTy, bodyTy)) => Right(argTy, bodyTy)
  case _ => Left(TypeCheckError(s"Expected $t to be a dependent product type", errCtx, ctx.snapshot))
}

private def [S](t: Whnf[S]) checkSigType()(given errCtx: ErrorContext[S])(given ctx: Context[S]): Either[TypeCheckError[S], (Term[S], Term[S])] = t match {
  case Val(Sig(aTy, bTy)) => Right(aTy, bTy)
  case _ => Left(TypeCheckError(s"Expected $t to be a dependent pair type.", errCtx, ctx.snapshot))
}

private def [S](t: Whnf[S]) checkSetType()(given errCtx: ErrorContext[S])(given ctx: Context[S]): Either[TypeCheckError[S], Int] = t match {
  case Val(Set(l)) => Right(l)
  case _ => Left(TypeCheckError(s"Expected $t to be a Set at some level.", errCtx, ctx.snapshot))
}
