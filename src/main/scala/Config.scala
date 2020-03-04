import output.ComplexSchema

object Config {
  type SchemaType = ComplexSchema /* YOUR SCHEMA TYPE HERE (generated POJO type) */

  def numOfRecords = 10

  def customRules: Seq[SchemaType] => Seq[SchemaType] = identity
}
