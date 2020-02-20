import runners.DefaultJsonRandomGeneratorRunner
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
    jsonRandomGeneratorRunner.run(clazz.newInstance(), numOfRecords)(type2TypeTag(mirror, typeSignature))
  }
}
