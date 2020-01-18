package io.github.tgeng.zealot.tt.frontend

import scala.language.implicitConversions
import io.github.tgeng.zealot.common.EitherSugar.given
import scala.collection.mutable.ArrayBuffer

case class ToStringSpec(idealLineWidth: Int = 80, indentSize: Int = 2)

class ToStringContext(spec: ToStringSpec) {

  private var indent: Int = 0
  private var currentLineStart = 0
  private val sb: StringBuilder = StringBuilder()
  private def currentLineWidth = sb.size - currentLineStart
  def remainingLineWidth = spec.idealLineWidth - currentLineWidth

  def newLine() : Unit = {
    // Trim all whitespace at the end of the current line
    var lastCharIndex = sb.size - 1
    while(lastCharIndex >= 0 && sb.charAt(lastCharIndex) == ' ') {
      lastCharIndex -= 1
    }
    sb.delete(lastCharIndex, sb.size)

    sb += '\n'
    currentLineStart = sb.size
    sb ++= " " * (spec.indentSize * indent)
  }

  def += (s: String) = sb ++= s

  inline def indentMore(action: => Unit) : Unit = {
    indent += 1
    action
    indent -= 1
  }

  inline def indentHere(action: => Unit) : Unit = {
    val oldIndent = indent
    indent = currentLineWidth
    action
    indent = oldIndent
  }

  def getString = sb.toString
}

def (ft: FTerm) prettyPrint(ctx: ToStringContext) : Unit =
  ft.toBlock().prettyPrint(given ctx)


private def(b: Block|String) prettyPrint(given ctx: ToStringContext) : Unit = b match {
  case s:String => ctx += s
  case b@Block(prefix, children, suffix) => {
    val insertNewLine = b.width > ctx.remainingLineWidth
    ctx += prefix
    ctx.indentMore {
      var first = true
      children.foreach { child =>
        if (first) {
          first == false
          if (prefix.nonEmpty && insertNewLine) {
            ctx.newLine()
          }
        } else if (insertNewLine) {
          ctx.newLine()
        }
        prettyPrint(child)
      }
    }
    if (suffix.nonEmpty && insertNewLine) {
      ctx.newLine()
    }
    ctx += suffix
  }
}

import FTerm._
import FReference._
import FValue._
import FRedux._

private def(ft: FTerm) toBlock(parentLevel: Int = 0): Block|String = {
  val result: Block|String = ft match {
    case FRef(FName(name)) => name
    case FVal(v) => v match {
      case FSet(l) => s"Set$l"
      case p: FPi => {
        val (doms, cod) = flatten[FPi, (String, FTerm), FTerm](
          p,
          p => (p.name, p.dom),
          p => p.cod match {
            case FVal(np@FPi(_, _, _)) => np
            case t => t
          })
        val children = ArrayBuffer[Block|String]()
        for (dom <- doms) {
          val (name, domType) = dom
          if (name.isEmpty) {
            children += domType.toBlock(ft.level + 1) <+> " -> "
          } else {
            children += s"($name : " <+> domType.toBlock() <+> ") -> "
          }
        }
        children += cod.toBlock(ft.level)
        Block("", children.toSeq, "")
      }
      case l: FLam => {
        val (names, body) = flatten[FLam, String, FTerm](
          l,
          _.name ,
          l => l.body match {
            case FVal(nl@FLam(_, _)) => nl
            case t => t
          }
        )

        Block("\\" + names.mkString(", ") + " => ", Seq(body.toBlock(ft.level)), "")
      }
      case s: FSig => {
        val (fsts, snd) = flatten[FSig, (String, FTerm), FTerm](
          s,
          s => (s.name, s.fstTy),
          s => s.sndTy match {
            case FVal(ns@FSig(_, _, _)) => ns
            case t => t
          })
        val children = ArrayBuffer[Block|String]()
        for (fst <- fsts) {
          val (name, fstTy) = fst
          if (name.isEmpty) {
            children += fstTy.toBlock(ft.level + 1) <+> " & "
          } else {
            children += s"($name : " <+> fstTy.toBlock() <+> ") & "
          }
        }
        children += snd.toBlock(ft.level)
        Block("", children.toSeq, "")
      }
      case p: FPair => {
        val (fsts, snd) = flatten[FPair, FTerm, FTerm](
          p,
          p => p.fst,
          p => p.snd match {
            case FVal(np@FPair(_, _)) => np
            case t => t
          })
        val children : Seq[Block|String] = fsts.map(t => t.toBlock(ft.level) <+> ", ")
        Block("", children :+ snd.toBlock(ft.level) , "")
      }
      case _: FUnit => "Unit"
      case _: FStar => "()"
    }
    case FRdx(r) => r match {
      case a: FApp => {
        val (args, fn) = flatten[FApp, FTerm, FTerm](
          a,
          a => a.arg,
          a => a.fn match {
            case FRdx(na@FApp(_, _)) => na
            case t => t
          })
        val childTerms = fn +: args.reverse
        val children : Seq[Block|String] = childTerms.zipWithIndex.map{(t, i) =>
          if (i == childTerms.size - 1) {
            t.toBlock(ft.level)
          } else {
            t.toBlock(ft.level) <+> " "
          }
        }
        Block("", children , "")
      }
      case FPrj1(pair) => pair.toBlock(ft.level) <+> ".1"
      case FPrj2(pair) => pair.toBlock(ft.level) <+> ".2"
    }
  }
  if (ft.level < parentLevel) {
    "(" <+> result <+> ")"
  } else {
    result
  }
}

private def (b: Block|String) <+> (s: String) : Block|String = b match {
  case Block(prefix, children, suffix) => Block(prefix, children, suffix + s)
  case bs: String => bs + s
}

private def (s: String) <+> (b: Block|String) : Block|String = b match {
  case Block(prefix, children, suffix) => Block(s + prefix, children, suffix)
  case bs: String => s + bs
}

private def(ft: FTerm) level : Int = ft match {
  case _: FRef => 10
  case FVal(v) => v match {
    case _: FSet => 10
    case _: FPi => 2
    case _: FLam => 1
    case _: FSig => 3
    case _: FPair => -1
    case _: FUnit => 10
    case _: FStar => 10
  }
  case FRdx(r) => r match {
    case _: FApp => 5
    case _: FPrj1 => 7
    case _: FPrj2 => 7
  }
}

private def flatten[T, E, R](t: T, eExtractor: T => E, tExtractor: T => Either[R, T]) : (Seq[E], R) = {
  val elems = ArrayBuffer[E]()
  var currentT = t;
  while(true) {
    elems += eExtractor(currentT)
    tExtractor(currentT) match {
      case Right(nextT) => {
        currentT = nextT
      }
      case Left(r) => return (elems.toSeq, r)
    }
  }
  throw AssertionError()
}

private case class Block(
  val prefix: String,
  val children: Seq[Block | String],
  val suffix: String,
) {
  private var _width: Int = -1
  def width : Int = {
    if (_width == -1) {
      _width = prefix.length + suffix.length + children.foldLeft(0){(acc, child) =>
        (child match {
          case b: Block => b.width
          case s: String => s.length
        }) + acc
      }
    }
    _width
  }
}