package generators.`type`

import org.scalacheck.{Gen, Arbitrary}

object MapTypeGenerators {
  object DefaultMapTypeGenerator extends TypeGenerator[Map[String, String]] {

    import Arbitrary._

    val keyGen: Gen[String] = Gen.identifier.map(_.take(10))
    val valGen: Gen[String] = Gen.oneOf(arbitrary[Number], arbitrary[Boolean], Gen.listOfN(10, Gen.alphaChar).map(_.mkString)).map(_.toString)

    override def apply(): Gen[Map[String, String]] = Gen.mapOfN(5, Gen.zip(keyGen, valGen))
  }
}
