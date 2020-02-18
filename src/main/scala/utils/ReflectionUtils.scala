package utils

import scala.reflect.api
import scala.reflect.runtime.universe._

object ReflectionUtils {
  def invokeMethod[P, R](obj: Any, arg: Object, methodName: String, paramType: Class[P]): R =
    obj.getClass.getMethod(methodName, paramType).invoke(obj, arg).asInstanceOf[R]

  def type2TypeTag[T](mirror: Mirror, tpe: Type): TypeTag[T] =
    TypeTag(mirror, new api.TypeCreator {
      def apply[U <: api.Universe with Singleton](m: api.Mirror[U]) =
        if (m eq mirror) tpe.asInstanceOf[U#Type]
        else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
    })
}
