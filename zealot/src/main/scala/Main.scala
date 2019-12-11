import io.github.tgeng.zealot.tt._

object Main {

  def main(args: Array[String]): Unit = {
    println("Hello world!")
    println(msg)
    val t = Term.Var(0)
  }

  def msg = "I was compiled by dotty :)"
}
