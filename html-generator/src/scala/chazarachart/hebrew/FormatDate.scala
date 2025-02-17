package chazarachart.hebrew

import jewishdate.{JewishDate, JewishMonth}

object FormatDate {
  private val hebrewMonths = Map(
    JewishMonth.Tishrei      -> "תשרי",
    JewishMonth.Cheshvan     -> "חשון",
    JewishMonth.Kislev       -> "כסלו",
    JewishMonth.Teves        -> "טבת",
    JewishMonth.Shvat        -> "שבט",
    JewishMonth.Adar         -> "אדר",
    JewishMonth.`Adar Sheni` -> "אדר ב'",
    JewishMonth.Nissan       -> "ניסן",
    JewishMonth.Iyar         -> "אייר",
    JewishMonth.Sivan        -> "סיון",
    JewishMonth.Tammuz       -> "תמוז",
    JewishMonth.Av           -> "אב",
    JewishMonth.Elul         -> "אלול"
  )

  def apply(date: JewishDate): String = {
    val day   = FormatNumber(date.dayOfMonth, plain = false)
    val month = hebrewMonths(date.month)
    val year  = FormatNumber(date.year.value % 1000, plain = false)
    s"$day $month $year"
  }
}
