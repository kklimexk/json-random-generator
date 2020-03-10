package runners

import generator.JsonRandomGenerator
import generators.`type`._

class DefaultJsonRandomGeneratorRunner extends JsonRandomGeneratorRunner {
  override val jsonRandomGenerator: JsonRandomGenerator = new JsonRandomGenerator(
    StringTypeGenerators.default,
    LongTypeGenerators.between(1, 100),
    BigDecimalTypeGenerators.between(1, 100),
    BooleanTypeGenerators.default,
    EnumTypeGenerators.default,
    MapTypeGenerators.default,
    ListTypeGenerators.defaultEnumListTypeGenerator,
    ListTypeGenerators.defaultStringListTypeGenerator(2),
    ListTypeGenerators.defaultLongListTypeGenerator(2, 1, 100),
    ListTypeGenerators.defaultBigDecimalListTypeGenerator(2, 1, 100),
    ListTypeGenerators.defaultBooleanListTypeGenerator(2)
  )
}
