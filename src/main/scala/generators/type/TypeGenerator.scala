package generators.`type`

import org.scalacheck.Gen

private[`type`] trait TypeGenerator[T] {
  def apply(): Gen[T]
}
