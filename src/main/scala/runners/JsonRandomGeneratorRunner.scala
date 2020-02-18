package runners

import scala.reflect.runtime.universe._

private[runners] trait JsonRandomGeneratorRunner {
  def run[A](topLevelObj: A)(implicit c: TypeTag[A]): Unit
}
