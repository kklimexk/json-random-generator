import runners.DefaultJsonRandomGeneratorRunner
import utils.JsonUtils.Implicits._
import utils.ReflectionUtils._

import scala.reflect.runtime.universe._

object Main {
  def main(args: Array[String]): Unit = {
    val rootClassName = "output.ComplexSchema"
    val numOfRecords = 10

    val clazz = Class.forName(rootClassName)
    val mirror = runtimeMirror(clazz.getClassLoader)

    val symbol = mirror.classSymbol(clazz)
    val typeSignature = symbol.typeSignature

    val jsonRandomGeneratorRunner = new DefaultJsonRandomGeneratorRunner()
    val generatedObjects = jsonRandomGeneratorRunner.generate(clazz.newInstance(), numOfRecords)(type2TypeTag(mirror, typeSignature))

    generatedObjects.save()
  }
}
