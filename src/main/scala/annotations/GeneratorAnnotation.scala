package annotations


sealed trait GeneratorAnnotation {
  def value: String
}

object GeneratorAnnotation {
  final case object ValueHintDecimal extends GeneratorAnnotation {
    val value = "ValueHintDecimal"
  }

  final case object ValueHintOptions extends GeneratorAnnotation {
    val value = "ValueHintOptions"
  }

  final case object ValueHintIterator extends GeneratorAnnotation {
    val value = "ValueHintIterator"
  }

  final case object ValueHintPrefix extends GeneratorAnnotation {
    val value = "ValueHintPrefix"
  }

  final case object ValueHintPostfix extends GeneratorAnnotation {
    val value = "ValueHintPostfix"
  }
}
