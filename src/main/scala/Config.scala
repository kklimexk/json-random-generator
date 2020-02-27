import custom.Mapping
import org.scalacheck.Gen
import output.ComplexSchema
import utils.DateTimeUtils

object Config {
  type SchemaType = ComplexSchema

  def numOfRecords = 10

  def generators(): Seq[Seq[SchemaType] => Seq[SchemaType]] = Seq(
    objects => Mapping(objects) { rows =>
      rows.zipWithIndex.map { case (r, idx) =>
        r.setCreatedDate(DateTimeUtils.date2Date(r.getCreatedDateTime))
        r.setCreatedTime(DateTimeUtils.date2Time(r.getCreatedDateTime))

        r.getPerson.setId(idx)
        r.getPerson.setName(Gen.oneOf("Gabriel", "Alicja", "Rafal", "Vova", "Milton", "Pawel").sample.get)
        r.getPerson.setLastname(Gen.oneOf("Kowalski", "Smith", "Brown", "Wilson", "Miller", "Johnson").sample.get)
      }
    }
  )
}
