package io.github.tgeng.zealot.parsec

import org.junit.Assert._
import io.github.tgeng.zealot.common._
import io.github.tgeng.fluentassert._
import io.github.tgeng.zealot.parsec.Parser

def testing[I, T](p: Parser[I, T])(tests: (given parser: Parser[I, T]) => Unit) = {
  tests(given p)
}

def [I, T](input: IndexedSeq[I]) succeedsWith (expected: T)(given parser: Parser[I, T]) : Unit = {
  try {
    parser.parse(input) match {
      case Right(actual) => (actual == expected) match {
        case true => ()
        case false => fail(
          getFailMessagePrefix(parser, input) + "expect output to be\n  " +
          expected.toString.indented(2) + "\nbut actual output is\n  " +
          actual.toString.indented(2) + "\n"
          )
      }
      case Left(e) => fail(
        getFailMessagePrefix(parser, input) + "expect output to be\n  " +
          expected.toString.indented(2) + "\nbut parsing fails with message\n  " +
          e.toString.indented(2) + "\n"
      )
    }
  } catch {
    case e => {
      println(getFailMessagePrefix(parser, input))
      e.printStackTrace
    }
  }
}

def [I, T](input: IndexedSeq[I]) failsWithMessage (message: String)(given parser: Parser[I, T]) : Unit = {
  try {
    val t = parser.parse(input)
    val trimmedMessage = message.trim.replaceAll("\n +", "\n")
    t match {
       case Right(t) => fail(getFailMessagePrefix(parser, input) +
         s"expect parsing to fail but it succeeds with\n  ${t.toString.indented(2)}\n")
       case Left(e) => e.toString() == trimmedMessage match {
         case false => fail(getFailMessagePrefix(parser, input) +
           s"expect parsing to fail with message\n  ${trimmedMessage.indented(2)}\nbut it fails with message\n  ${e.toString().indented(2)}\n")
         case _ => ()
       }
    }
  } catch {
    case e => {
      println(getFailMessagePrefix(parser, input))
      e.printStackTrace
    }
  }
}


private def getFailMessagePrefix(parser: Parser[?, ?], input: Any) =
  s"\nWith ${parser.toString} and given input\n  " +
  input.toString.indented(2) + "\n"
