package generators.`type`

import org.scalacheck.Gen

object StringTypeGenerators {
  object DefaultStringTypeGenerator extends TypeGenerator[String] {
    override def apply(): Gen[String] = Gen.listOfN(10, Gen.alphaChar).map(_.mkString)
  }
}
