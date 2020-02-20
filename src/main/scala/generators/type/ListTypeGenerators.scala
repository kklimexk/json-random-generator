package generators.`type`

import java.util

import org.scalacheck.Gen

import scala.collection.JavaConverters._

object ListTypeGenerators {

  object DefaultEnumListTypeGenerator {
    def apply(): (Int, Array[Any]) => Gen[util.List[Any]] = (numOfElem: Int, enumValues: Array[Any]) =>
      Gen.listOfN(numOfElem, Gen.oneOf(enumValues)).map(_.toList).map(_.asJava)
  }
}
