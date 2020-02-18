package generators.`type`

import org.scalacheck.Gen

object DoubleTypeGenerators {
  object DefaultDoubleTypeGenerator extends TypeGenerator[java.lang.Double] {
    override def apply(): Gen[java.lang.Double] = Gen.choose(1.0,100.0).map(i => new java.lang.Double(i))
  }
}
