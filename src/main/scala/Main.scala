import runners.DefaultJsonRandomGeneratorRunner
import utils.JsonUtils.Implicits._
import utils.ReflectionUtils._
import utils._

import scala.reflect.runtime.universe._

object Main {
  def main(args: Array[String]): Unit = {
    val argsOpt = args.lift

    val clazzName = argsOpt(0).getOrElse(Config.schemaPath)

    val clazz = Class.forName(clazzName)
    val mirror = runtimeMirror(clazz.getClassLoader)

    val symbol = mirror.classSymbol(clazz)

    val typeSignature = symbol.typeSignature

    val jsonRandomGeneratorRunner = new DefaultJsonRandomGeneratorRunner()
    executionTime {
      val generatedObjects = jsonRandomGeneratorRunner.generate(clazz, argsOpt(1).map(_.toInt).getOrElse(Config.numOfRecords))(type2TypeTag(mirror, typeSignature))
      val res = Config.customRules(generatedObjects.asInstanceOf[Seq[Config.SchemaType]])

      val targetFileName = s"${res.head.getClass.getSimpleName}.json"

      res.save(argsOpt(2).map(p => s"$p/$targetFileName").getOrElse(s"target/$targetFileName"))
    }
  }
}
