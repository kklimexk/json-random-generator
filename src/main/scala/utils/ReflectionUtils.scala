package utils

import java.util

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

          if (methodFullName.startsWith("output") &&
            methodFullName.contains("get")) {
            println(s"$methodName, $methodReturnType")

            val returnTypeSymbol = methodReturnType.typeSymbol.asType

            Class.forName(returnTypeSymbol.fullName) match {
              case t if t == classOf[Object] =>
                println(t)
                invokeMethod(obj, null,
                  methodName.toString.replaceFirst("g", "s"), t)
              case t if t == classOf[String] =>
                println(t)
                invokeMethod(obj, "ExampleString",
                  methodName.toString.replaceFirst("g", "s"), t)
              case t if t == classOf[java.lang.Integer] =>
                println(t)
                invokeMethod(obj, new java.lang.Integer(30),
                  methodName.toString.replaceFirst("g", "s"), t)
              case t if t == classOf[java.lang.Double] =>
                println(t)
                invokeMethod(obj, new java.lang.Double(11.1),
                  methodName.toString.replaceFirst("g", "s"), t)
              case t if t == classOf[java.lang.Boolean] =>
                println(t)
                invokeMethod(obj, new java.lang.Boolean(false),
                  methodName.toString.replaceFirst("g", "s"), t)
              case t if t == classOf[java.util.List[_]] =>
                println(t)
                invokeMethod(obj, new util.ArrayList(),
                  methodName.toString.replaceFirst("g", "s"), t)
              case t =>
                val newInstance = Class.forName(returnTypeSymbol.fullName).newInstance()
                invokeMethod(obj, newInstance.asInstanceOf[AnyRef],
                  methodName.toString.replaceFirst("g", "s"), t)
                loop(newInstance, methodReturnType)
                //loop(mirror.reflect(methodReturnType).instance, methodReturnType)
            }
          }
        }
      }
      topLevelObj
    }

    loop(topLevelObj, c.tpe)
  }
}
