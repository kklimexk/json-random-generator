package generators.`type`

import org.scalacheck.Gen

object LongTypeGenerators {
  def between: (Long, Long) => Gen[java.lang.Long] = (from: Long, to: Long) =>
    Gen.choose(from, to).map(i => new java.lang.Long(i))
}
