package generators.`type`

import org.scalacheck.Gen

object DoubleTypeGenerators {
  def default: Gen[java.lang.Double] = Gen.choose(1.0,100.0).map(i => new java.lang.Double(i))
}
