package chazarachart.hebrew

object FormatNumber extends App {
  private lazy val ones     = "אבגדהוזחט"
  private lazy val tens     = "יכלמנסעפצ"
  private lazy val hundreds = "קרשת"

  private def impl(n: Int): String = {
    if (n >= 400)
      "ת" + impl(n - 400)
    else if (n >= 100)
      hundreds.charAt(n / 100 - 1).toString + impl(n % 100)
    else if (n >= 10)
      tens.charAt(n / 10 - 1).toString + impl(n % 10)
    else if (n >= 1)
      ones.charAt(n - 1).toString
    else
      ""
  }

  def apply(n: Int, plain: Boolean): String = {
    require(
      n >= 0 && n <= 1000,
      s"Number must be between 0 and 1000 inclusive but was $n"
    )
    val str = impl(n)
    if (plain || str.isEmpty)
      str
    else if (str.length == 1)
      str + "'"
    else
      str.patch(str.length - 1, "\"", 0)
  }

  for (i <- 0 to 1000)
    println(i + " -> " + apply(i, plain = false))
}
