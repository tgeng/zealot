import io.github.tgeng.zealot.tt._

object Main {

  def main(args: Array[String]): Unit = {
    val l = Term.Lam(Term.Var(1))
    val s = Term.Var(100)
    val lRaised = l.raise()
    val lSub = l.substitute(0, s)
    println(l)
    println(lRaised)
    println(lSub)
  }
}
