package generators.`type`

import org.scalacheck.Gen

object StringTypeGenerators {
  def default: Gen[String] = Gen.listOfN(10, Gen.alphaChar).map(_.mkString)
}
