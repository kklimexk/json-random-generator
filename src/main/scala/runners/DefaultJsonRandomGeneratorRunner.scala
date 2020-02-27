package runners

import generator.JsonRandomGenerator
import generators.`type`._

class DefaultJsonRandomGeneratorRunner extends JsonRandomGeneratorRunner {
  override val jsonRandomGenerator: JsonRandomGenerator = new JsonRandomGenerator(
    StringTypeGenerators.default,
    IntegerTypeGenerators.default,
    DoubleTypeGenerators.default,
    BooleanTypeGenerators.default,
    EnumTypeGenerators.default,
    MapTypeGenerators.default,
    ListTypeGenerators.defaultEnumListTypeGenerator,
    ListTypeGenerators.defaultStringListTypeGenerator,
    ListTypeGenerators.defaultIntegerListTypeGenerator,
    ListTypeGenerators.defaultDoubleListTypeGenerator,
    ListTypeGenerators.defaultBooleanListTypeGenerator
  )
}
