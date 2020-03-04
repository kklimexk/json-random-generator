import runners.DefaultJsonRandomGeneratorRunner
import utils.JsonUtils.Implicits._
import utils.ReflectionUtils._
import utils._

import scala.reflect.runtime.universe._

object Main {
  def main(args: Array[String]): Unit = {
    val clazz = createInstanceFromType[Config.SchemaType].getClass
    val mirror = runtimeMirror(clazz.getClassLoader)

    val symbol = mirror.classSymbol(clazz)
    val typeSignature = symbol.typeSignature

    val jsonRandomGeneratorRunner = new DefaultJsonRandomGeneratorRunner()
    executionTime {
      val generatedObjects = jsonRandomGeneratorRunner.generate(clazz, Config.numOfRecords)(type2TypeTag(mirror, typeSignature))

      val res = Config.customRules(generatedObjects)

      val targetFileName = s"${res.head.getClass.getSimpleName}.json"

      res.save(args.headOption.map(p => s"$p/$targetFileName").getOrElse(s"target/$targetFileName"))
    }
  }
}
