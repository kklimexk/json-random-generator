

object Config {
  type SchemaType /* YOUR SCHEMA TYPE HERE (generated POJO type) */

  def numOfRecords = 10

  def customRules: Seq[SchemaType] => Seq[SchemaType] = identity
}
