package generators.`type`

import java.util.Date

import org.scalacheck.Gen

object DateTypeGenerators {
  def between: (Date, Date) => Gen[java.util.Date] = (from: Date, to: Date) =>
    Gen.choose(from.getTime, to.getTime).map(time => new Date(time))
}
