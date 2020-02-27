package utils

import java.text.SimpleDateFormat
import java.util.{Date, TimeZone}

object DateTimeUtils {
  def date2DateTime(date: Date): String = formatDate(date, "yyyy-MM-dd'T'HH:mm:ss.SSSXXX")

  def date2Date(date: Date): String = formatDate(date, "yyyy-MM-dd")

  def date2Time(date: Date): String = formatDate(date, "HH:mm:ss.SSS")

  def formatDate(date: Date, format: String): String = {
    val formatter = new SimpleDateFormat(format)
    formatter.setTimeZone(TimeZone.getTimeZone("UTC"))
    val dateFormatted = formatter.format(date)
    dateFormatted
  }
}
