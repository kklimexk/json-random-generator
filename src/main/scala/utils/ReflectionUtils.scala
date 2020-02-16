package utils

import java.util

import scala.reflect.runtime.universe._

object ReflectionUtils {

  def invokeMethod[P, R](obj: Any, arg: Object, methodName: String, paramType: Class[P]): R =
    obj.getClass.getMethod(methodName, paramType).invoke(obj, arg).asInstanceOf[R]

  def getEnumValues[E](enumClass: Class[E]): Array[E] = {
    val f = enumClass.getDeclaredField("$VALUES")
    f.setAccessible(true)
    val o = f.get(null)

    o.asInstanceOf[Array[E]]
  }

  def evalMemberValues[A](topLevelObj: A)(implicit c: TypeTag[A]): A = {

    val mirror = runtimeMirror(getClass.getClassLoader)

    def loop(obj: Any, tp: Type): A = {
      //println(s"INSPECTING: $tp:")
      val objMirror = mirror.reflect(obj)
      val members = tp.decls.filter(_.isPublic)
      members.foreach { m =>
        /*if (m.isTerm && m.isModule) {
          println(s"MODULE: $m")
          loop(mirror.reflectModule(m.asModule).instance, m.info)
        }
        else if (m.isTerm && !m.isConstructor && m.isMethod && m.typeSignature.paramLists.isEmpty && !m.typeSignature.takesTypeArgs) {
          val value = objMirror.reflectMethod(m.asMethod)()
          println(s"VAL/DEF: $m = $value, type: ${m.typeSignature}")
        }*/
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
}