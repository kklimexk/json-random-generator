package utils

import scala.reflect.runtime.universe._

object ReflectionUtils {
  def evalMemberValues[A](topLevelObj: A)(implicit c: TypeTag[A]): Unit = {

    val mirror = runtimeMirror(getClass.getClassLoader)

    def loop(obj: Any, tp: Type): Unit = {
      println(s"INSPECTING: $tp:")
      val objMirror = mirror.reflect(obj)
      val members = tp.decls.filter(_.isPublic)
      members.foreach { m =>
        if (m.isTerm && m.isModule) {
          println(s"MODULE: $m")
          loop(mirror.reflectModule(m.asModule).instance, m.info)
        }
        else if (m.isTerm && !m.isConstructor && m.isMethod && m.typeSignature.paramLists.isEmpty && !m.typeSignature.takesTypeArgs) {
          val value = objMirror.reflectMethod(m.asMethod)()
          println(s"VAL/DEF: $m = $value, type: ${m.typeSignature}")
        }
        else {
          println(s"OTHERS: $m")

        }
      }
    }

    loop(topLevelObj, c.tpe)
  }
}
