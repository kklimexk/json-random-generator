package generators.`type`

import org.scalacheck.Gen

object EnumTypeGenerators {
  def default: Array[Any] => Gen[Any] = (possibleValues: Array[Any]) => Gen.oneOf(possibleValues)
}
