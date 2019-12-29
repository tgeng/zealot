package io.github.tgeng.zealot.tt

import scala.util.Either
import io.github.tgeng.zealot.tt.Whnf._
import io.github.tgeng.zealot.tt.Value._

def (t: Term) checkType(ty: Type)(given ctx: Context) : Either[TypeCheckError, Unit] = try {
  (t.whnf, ty) match {
    case (Val(Lam(body)), Val(Pi(argTy, bodyTy))) => {
      (ctx :: argTy.whnf) { () => 
        body.checkType(bodyTy.whnf) 
      }
    }
    case (Val(Pair(a, b)), Val(Sig(aTy, bTy))) => {
      val aTyWhnf = aTy.whnf
      for {
        _ <- a.checkType(aTyWhnf)
        result <- b.checkType(bTy.substitute(0, aTyWhnf.term).whnf)
      } yield result
    }
    case _ => for {
      inferredType <- t.inferType()
      _ <- inferredType <= ty
    } yield ()
  }
} catch {
  case e : WhnfStuckException => Left(e.toTypeCheckError())
}

def (t: Term) inferType()(given ctx: Context) : Either[TypeCheckError, Whnf] = {
  throw UnsupportedOperationException()
}

def (a: Whnf) <= (b: Whnf) (given ctx: Context) : Either[TypeCheckError, Unit] = {
  throw UnsupportedOperationException()
}

class TypeCheckError(message: String)

private def (e: WhnfStuckException) toTypeCheckError() = TypeCheckError(e.getMessage)