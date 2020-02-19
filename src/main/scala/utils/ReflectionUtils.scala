package utils

import scala.reflect.api
import scala.reflect.runtime.universe._

object ReflectionUtils {
  def invokeMethod[R](obj: Any, args: Seq[Object], methodName: String, paramTypes: Seq[Class[_]]): R =
    obj.getClass.getMethod(methodName, paramTypes: _*).invoke(obj, args: _*).asInstanceOf[R]

  def type2TypeTag[T](mirror: Mirror, tpe: Type): TypeTag[T] =
    TypeTag(mirror, new api.TypeCreator {
      def apply[U <: api.Universe with Singleton](m: api.Mirror[U]) =
        if (m eq mirror) tpe.asInstanceOf[U#Type]
        else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
    })

  def getAnnotationProperties(annotation: Annotation): (String, Map[String, String]) = {
    val returnValue = annotation.tree.children
      .filter(a => a.isInstanceOf[AssignOrNamedArg])
      .map(_.asInstanceOf[AssignOrNamedArg])
      .map(ap => (ap.lhs, ap.rhs))

    annotationTypeName(annotation) -> returnValue.map { case (annParam, annValue) => annParam.toString() -> annValue.toString()}.toMap
  }

  def annotationTypeName(annotation: Annotation): String = annotation.tree.tpe.typeSymbol.name.toString
}
