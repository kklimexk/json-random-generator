package generators.`type`

import org.scalacheck.Gen

object EnumTypeGenerators {
  private[`type`] sealed trait EnumTypeGenerator[T] {
    def apply(): Array[T] => Gen[T]
  }

  object DefaultEnumTypeGenerator extends EnumTypeGenerator[Any] {
    override def apply(): Array[Any] => Gen[Any] = (possibleValues: Array[Any]) => Gen.oneOf(possibleValues)
  }
}
