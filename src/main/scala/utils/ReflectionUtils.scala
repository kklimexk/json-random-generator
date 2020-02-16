package utils

object ReflectionUtils {

  def invokeMethod[P, R](obj: Any, arg: Object, methodName: String, paramType: Class[P]): R =
    obj.getClass.getMethod(methodName, paramType).invoke(obj, arg).asInstanceOf[R]
}
