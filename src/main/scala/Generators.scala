import Generators.ResultType
import custom.Mapping
import output.ComplexSchema

class Generators {
  type Mapping = Seq[ResultType] => Seq[ResultType]

  def generators(): Seq[Mapping] = Seq(
    objects => Mapping(objects) { rows =>
      rows.map { r =>
        r.setStringField("String Field!!!")
        r.setShippingAddress("Shipping Address!!!")
      }
    }
  )
}

object Generators {
  type ResultType = ComplexSchema
}
