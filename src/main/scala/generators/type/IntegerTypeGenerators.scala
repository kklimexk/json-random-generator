package generators.`type`

import org.scalacheck.Gen

object IntegerTypeGenerators {
  object DefaultIntegerTypeGenerator extends TypeGenerator[java.lang.Integer] {
    override def apply(): Gen[java.lang.Integer] = Gen.choose(1,100).map(i => new Integer(i))
  }
}
