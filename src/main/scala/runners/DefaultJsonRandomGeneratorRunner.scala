package runners

import generator.JsonRandomGenerator
import generators.`type`.BooleanTypeGenerators.DefaultBooleanTypeGenerator
import generators.`type`.DoubleTypeGenerators.DefaultDoubleTypeGenerator
import generators.`type`.EnumTypeGenerators.DefaultEnumTypeGenerator
import generators.`type`.IntegerTypeGenerators.DefaultIntegerTypeGenerator
import generators.`type`.MapTypeGenerators.DefaultMapTypeGenerator
import generators.`type`.StringTypeGenerators.DefaultStringTypeGenerator

class DefaultJsonRandomGeneratorRunner extends JsonRandomGeneratorRunner {
  override val jsonRandomGenerator: JsonRandomGenerator = new JsonRandomGenerator(
    DefaultStringTypeGenerator(),
    DefaultIntegerTypeGenerator(),
    DefaultDoubleTypeGenerator(),
    DefaultBooleanTypeGenerator(),
    DefaultEnumTypeGenerator(),
    DefaultMapTypeGenerator()
  )
}
