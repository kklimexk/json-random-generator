package generators.`type`

import java.math.{BigInteger, MathContext}

import org.scalacheck.Gen

object BigDecimalTypeGenerators {
  def between: (Int, Int, Int, Int) => Gen[java.math.BigDecimal] = (from: Int, to: Int, precision: Int, scale: Int) =>
    Gen.choose(from, to).map(i => new java.math.BigDecimal(new BigInteger(i.toString), scale, new MathContext(precision)))
}
