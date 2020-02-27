package generators.`type`

import org.scalacheck.Gen

object BooleanTypeGenerators {
  def default: Gen[java.lang.Boolean] = Gen.oneOf(true, false).map(b => new java.lang.Boolean(b))
}
