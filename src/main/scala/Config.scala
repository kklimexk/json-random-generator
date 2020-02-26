import custom.Mapping
import output.ComplexSchema

object Config {
  type ResultType = ComplexSchema

  def numOfRecords = 10

  def generators(): Seq[Seq[ResultType] => Seq[ResultType]] = Seq(
    objects => Mapping(objects) { rows =>
      rows.map { r =>
        r.setStringField("String Field!!!")
        r.setShippingAddress("Shipping Address!!!")
      }
    }
  )
}
