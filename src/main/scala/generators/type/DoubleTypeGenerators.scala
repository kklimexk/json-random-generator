package generators.`type`

import org.scalacheck.Gen

object DoubleTypeGenerators {
  def between: (Double, Double) => Gen[java.lang.Double] = (from: Double, to: Double) =>
    Gen.choose(from, to).map(i => new java.lang.Double(i))
}
