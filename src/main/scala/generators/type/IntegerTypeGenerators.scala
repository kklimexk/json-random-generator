package generators.`type`

import org.scalacheck.Gen

object IntegerTypeGenerators {
  def between: (Int, Int) => Gen[java.lang.Integer] = (from: Int, to: Int) =>
    Gen.choose(from, to).map(i => new Integer(i))
}
