

object Config {
  type SchemaType = AnyRef /* YOUR SCHEMA TYPE HERE (generated POJO type) */

  def schemaPath = ""

  def numOfRecords = 10

  def customRules: Seq[SchemaType] => Seq[SchemaType] = identity
}
