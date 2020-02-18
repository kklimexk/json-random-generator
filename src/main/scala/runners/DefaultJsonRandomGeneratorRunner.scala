package runners

import java.io.File

import com.fasterxml.jackson.databind.{ObjectMapper, SerializationFeature}
import generator.JsonRandomGenerator
import generators.`type`.BooleanTypeGenerators.DefaultBooleanTypeGenerator
import generators.`type`.DoubleTypeGenerators.DefaultDoubleTypeGenerator
import generators.`type`.EnumTypeGenerators.DefaultEnumTypeGenerator
import generators.`type`.IntegerTypeGenerators.DefaultIntegerTypeGenerator
import generators.`type`.StringTypeGenerators.DefaultStringTypeGenerator

import scala.reflect.runtime.universe._

class DefaultJsonRandomGeneratorRunner extends JsonRandomGeneratorRunner {
  override def run[A](topLevelObj: A)(implicit c: TypeTag[A]): Unit = {
    val jsonObj = new JsonRandomGenerator(
      DefaultStringTypeGenerator(),
      DefaultIntegerTypeGenerator(),
      DefaultDoubleTypeGenerator(),
      DefaultBooleanTypeGenerator(),
      DefaultEnumTypeGenerator()
    ).generate(topLevelObj)

    println(jsonObj)

    val mapper = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT)
    mapper.writeValue(new File(s"target/${jsonObj.getClass.getSimpleName}.json"), jsonObj)
    val jsonString = mapper.writeValueAsString(jsonObj)

    println(jsonString)
  }
}
