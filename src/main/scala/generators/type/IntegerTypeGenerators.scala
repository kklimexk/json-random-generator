package generators.`type`

import org.scalacheck.Gen

object IntegerTypeGenerators {
  def default: Gen[java.lang.Integer] = Gen.choose(1,100).map(i => new Integer(i))
}
