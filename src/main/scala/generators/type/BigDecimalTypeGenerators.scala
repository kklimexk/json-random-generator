package generators.`type`

import java.math.{BigInteger, MathContext}

import org.scalacheck.Gen

object BigDecimalTypeGenerators {
  def between: (Long, Long, Int, Int) => Gen[java.math.BigDecimal] = (from: Long, to: Long, precision: Int, scale: Int) =>
    Gen.choose(from, to).map(i => new java.math.BigDecimal(new BigInteger(i.toString), scale, new MathContext(precision)))
}
