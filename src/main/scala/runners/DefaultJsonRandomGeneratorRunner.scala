package runners

import generator.JsonRandomGenerator
import generators.`type`._

class DefaultJsonRandomGeneratorRunner extends JsonRandomGeneratorRunner {
  override val jsonRandomGenerator: JsonRandomGenerator = new JsonRandomGenerator(
    StringTypeGenerators.default,
    IntegerTypeGenerators.between(1, 100),
    DoubleTypeGenerators.between(1.00, 100.0),
    BooleanTypeGenerators.default,
    EnumTypeGenerators.default,
    MapTypeGenerators.default,
    ListTypeGenerators.defaultEnumListTypeGenerator,
    ListTypeGenerators.defaultStringListTypeGenerator,
    ListTypeGenerators.defaultIntegerListTypeGenerator(2, 1, 100),
    ListTypeGenerators.defaultDoubleListTypeGenerator(2, 1.0, 100.0),
    ListTypeGenerators.defaultBooleanListTypeGenerator
  )
}
