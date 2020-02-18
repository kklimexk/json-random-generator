import output.ComplexSchema
import runners.DefaultJsonRandomGeneratorRunner

object Main {
  def main(args: Array[String]): Unit = {

    val jsonRandomGeneratorRunner = new DefaultJsonRandomGeneratorRunner()
    jsonRandomGeneratorRunner.run(new ComplexSchema())
  }
}
