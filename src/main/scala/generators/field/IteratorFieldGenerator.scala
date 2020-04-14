package generators.field

import scala.collection.mutable

final case class IteratorField(fullName: String, state: Long)

object IteratorFieldGenerator {
  private[this] val iteratorsMap = mutable.Map.empty[String, IteratorField]

  def next(iterator: IteratorField, step: Long): IteratorField = {
    iteratorsMap.find(_._1 == iterator.fullName) match {
      case Some((fieldName, it)) =>
        val updatedIterator = it.copy(state = it.state + step)
        iteratorsMap += fieldName -> updatedIterator
        updatedIterator
      case _ =>
        iteratorsMap += iterator.fullName -> iterator
        iterator
    }
  }
}
