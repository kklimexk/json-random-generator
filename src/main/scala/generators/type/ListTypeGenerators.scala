package generators.`type`

import java.util

import org.scalacheck.Gen

import scala.collection.JavaConverters._

object ListTypeGenerators {

  def defaultEnumListTypeGenerator: (Int, Array[Any]) => Gen[util.List[Any]] = (numOfElem: Int, enumValues: Array[Any]) =>
    Gen.listOfN(numOfElem, EnumTypeGenerators.default(enumValues)).map(_.asJava)

  def defaultStringListTypeGenerator: Int => Gen[util.List[String]] = (numOfElem: Int) =>
    Gen.listOfN(numOfElem, StringTypeGenerators.default).map(_.asJava)

  def defaultIntegerListTypeGenerator: (Int, Int, Int) => Gen[util.List[java.lang.Integer]] = (numOfElem: Int, from: Int, to: Int) =>
    Gen.listOfN(numOfElem, IntegerTypeGenerators.between(from, to)).map(_.asJava)

  def defaultBigDecimalListTypeGenerator: (Int, Int, Int, Int, Int) => Gen[util.List[java.math.BigDecimal]] = (numOfElem: Int, from: Int, to: Int, precision: Int, scale: Int) =>
    Gen.listOfN(numOfElem, BigDecimalTypeGenerators.between(from, to, precision, scale)).map(_.asJava)

  def defaultBooleanListTypeGenerator: Int => Gen[util.List[java.lang.Boolean]] = (numOfElem: Int) =>
    Gen.listOfN(numOfElem, BooleanTypeGenerators.default).map(_.asJava)
}
