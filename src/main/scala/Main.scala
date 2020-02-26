import runners.DefaultJsonRandomGeneratorRunner
import utils.JsonUtils.Implicits._
import utils.ReflectionUtils._

import scala.reflect.runtime.universe._

object Main {
  def main(args: Array[String]): Unit = {
    val clazz = createInstanceFromType[Config.ResultType].getClass
    val mirror = runtimeMirror(clazz.getClassLoader)

    val symbol = mirror.classSymbol(clazz)
    val typeSignature = symbol.typeSignature

    val jsonRandomGeneratorRunner = new DefaultJsonRandomGeneratorRunner()
    val generatedObjects = jsonRandomGeneratorRunner.generate(clazz, Config.numOfRecords)(type2TypeTag(mirror, typeSignature))

    val res = Config.generators().foldLeft(generatedObjects.asInstanceOf[Seq[Config.ResultType]])((el, transformation) => transformation(el))

    res.save()
  }
}
