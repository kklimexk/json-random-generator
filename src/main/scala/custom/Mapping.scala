package custom

object Mapping {
  def apply[A](objects: Seq[A])(generator: Seq[A] => Unit): Seq[A] = {
    generator(objects)

    objects
  }
}
