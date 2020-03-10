package annotations


sealed trait GeneratorAnnotation {
  def value: String
}

object GeneratorAnnotation {
  final case object ValueHintDecimal extends GeneratorAnnotation {
    val value = "ValueHintDecimal"
  }
}
