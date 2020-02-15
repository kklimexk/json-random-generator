package utils

import scala.reflect._
import scala.reflect.runtime.universe._

object ReflectionUtils {

  def invokeMethod[P, R](obj: Any, arg: Object, methodName: String, paramType: Class[P]): R =
    obj.getClass.getMethod(methodName, paramType).invoke(obj, arg).asInstanceOf[R]

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

          /*println("Example: " + methodReturnType)
          methodReturnType match {
            case _ if methodFullName.startsWith("com.example.types") &&
              methodFullName.contains("get") =>
              println(s"$methodName, $methodReturnType")
              loop(mirror.reflect(methodReturnType).instance, methodReturnType)
          }*/

          if (methodFullName.startsWith("output") &&
            methodFullName.contains("get")) {
            println(s"$methodName, $methodReturnType")

            val returnTypeSymbol = methodReturnType.typeSymbol.asType

            Class.forName(returnTypeSymbol.fullName) match {
              case t if t == classOf[Object] =>
                println(t)
              case t if t == classOf[String] =>
                println("String")
                invokeMethod(obj, "ExampleString",
                  methodName.toString.replaceFirst("g", "s"), t)
              case _ =>
            }

            loop(Class.forName(returnTypeSymbol.fullName).newInstance(),
              methodReturnType)
            //loop(mirror.reflect(methodReturnType).instance, methodReturnType)
          }
        }
      }
      topLevelObj
    }

    loop(topLevelObj, c.tpe)
  }
}
