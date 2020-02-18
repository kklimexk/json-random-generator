package generators.`type`

import org.scalacheck.Gen

object BooleanTypeGenerators {
  object DefaultBooleanTypeGenerator extends TypeGenerator[java.lang.Boolean] {
    override def apply(): Gen[java.lang.Boolean] = Gen.oneOf(true, false).map(b => new java.lang.Boolean(b))
  }
}
