package generator

import java.util

import utils.ReflectionUtils._

import scala.reflect.runtime.universe._

object JsonRandomGenerator {
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
              if (typeS.isJavaEnum)
                typeS.fullName.split("\\.").mkString("$").replaceFirst("\\$", ".")
              else typeS.fullName
            }

            try {
              Class.forName(returnTypeSymbolFullName) match {
                case t if t == classOf[Object] =>
                  invokeMethod(obj, null,
                    methodName.toString.replaceFirst("g", "s"), t)
                case t if t == classOf[String] =>
                  invokeMethod(obj, "ExampleString",
                    methodName.toString.replaceFirst("g", "s"), t)
                case t if t == classOf[java.lang.Integer] =>
                  invokeMethod(obj, new java.lang.Integer(30),
                    methodName.toString.replaceFirst("g", "s"), t)
                case t if t == classOf[java.lang.Double] =>
                  invokeMethod(obj, new java.lang.Double(11.1),
                    methodName.toString.replaceFirst("g", "s"), t)
                case t if t == classOf[java.lang.Boolean] =>
                  invokeMethod(obj, new java.lang.Boolean(false),
                    methodName.toString.replaceFirst("g", "s"), t)
                case t if t.isEnum =>
                  val enumValues = getEnumValues(t)
                  invokeMethod(obj, enumValues.head.asInstanceOf[AnyRef],
                    methodName.toString.replaceFirst("g", "s"), t)
                case t if t == classOf[java.util.List[_]] =>
                  invokeMethod(obj, new util.ArrayList(),
                    methodName.toString.replaceFirst("g", "s"), t)
                case t =>
                  val newInstance = Class.forName(returnTypeSymbolFullName).newInstance()
                  invokeMethod(obj, newInstance.asInstanceOf[AnyRef],
                    methodName.toString.replaceFirst("g", "s"), t)
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
