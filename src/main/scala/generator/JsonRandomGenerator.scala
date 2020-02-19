package generator

import java.util

import org.scalacheck.Gen
import utils.ReflectionUtils._

import scala.reflect.runtime.universe._

class JsonRandomGenerator(strGen: Gen[String],
                          intGen: Gen[java.lang.Integer],
                          doubleGen: Gen[java.lang.Double],
                          booleanGen: Gen[java.lang.Boolean],
                          enumGen: Array[Any] => Gen[Any]) {
  def generate[A](topLevelObj: A)(implicit c: TypeTag[A]): A = {

    def loop(obj: Any, tp: Type): A = {
      val members = tp.decls.filter(_.isPublic)
      members.foreach { m =>
        if (m.isMethod) {
          val methodFullName = m.asMethod.fullName
          val methodName = m.asMethod.name
          val methodReturnType = m.asMethod.returnType

          if (methodFullName.startsWith("output") &&
            methodFullName.contains("get")) {
            println(s"$methodName, $methodReturnType")

            val returnTypeSymbolFullName = {
              val typeS = methodReturnType.typeSymbol.asType
              val typeSFullName = typeS.fullName

              val typeSArr = typeSFullName.split("\\.").toList
              val isInnerEnum = typeSArr.lift(typeSArr.size - 2).exists(_.head.isUpper)

              if (typeS.isJavaEnum && isInnerEnum)
                typeS.fullName.patch(typeS.fullName.lastIndexOf('.'), "$", 1)
              else typeS.fullName
            }

            try {
              Class.forName(returnTypeSymbolFullName) match {
                case t if t == classOf[Object] =>
                  invokeMethod(obj, Seq(null),
                    methodName.toString.replaceFirst("g", "s"), Seq(t))
                case t if t == classOf[String] =>
                  invokeMethod(obj, Seq(strGen.sample.get),
                    methodName.toString.replaceFirst("g", "s"), Seq(t))
                case t if t == classOf[java.lang.Integer] =>
                  invokeMethod(obj, Seq(intGen.sample.get),
                    methodName.toString.replaceFirst("g", "s"), Seq(t))
                case t if t == classOf[java.lang.Double] =>
                  invokeMethod(obj, Seq(doubleGen.sample.get),
                    methodName.toString.replaceFirst("g", "s"), Seq(t))
                case t if t == classOf[java.lang.Boolean] =>
                  invokeMethod(obj, Seq(booleanGen.sample.get),
                    methodName.toString.replaceFirst("g", "s"), Seq(t))
                case t if t.isEnum =>
                  val enumValues = getEnumValues(t.asInstanceOf[Class[Any]])
                  invokeMethod(obj, Seq(enumGen(enumValues).sample.get.asInstanceOf[AnyRef]),
                    methodName.toString.replaceFirst("g", "s"), Seq(t))
                case t if t == classOf[java.util.List[_]] =>
                  invokeMethod(obj, Seq(new util.ArrayList()),
                    methodName.toString.replaceFirst("g", "s"), Seq(t))
                case t if t == classOf[java.util.Map[_, _]] =>
                  invokeMethod(obj, Seq(new util.HashMap()),
                    methodName.toString.replaceFirst("g", "s"), Seq(t))
                case t =>
                  val newInstance = Class.forName(returnTypeSymbolFullName).newInstance()
                  invokeMethod(obj, Seq(newInstance.asInstanceOf[AnyRef]),
                    methodName.toString.replaceFirst("g", "s"), Seq(t))
                  loop(newInstance, methodReturnType)
                //loop(mirror.reflect(methodReturnType).instance, methodReturnType)
              }
            } catch {
              case e: ClassNotFoundException => e.printStackTrace()
            }
          }
        }
      }
      topLevelObj
    }

    loop(topLevelObj, c.tpe)
  }

  private def getEnumValues[E](enumClass: Class[E]): Array[E] = {
    val f = enumClass.getDeclaredField("$VALUES")
    f.setAccessible(true)
    val o = f.get(null)

    o.asInstanceOf[Array[E]]
  }
}
