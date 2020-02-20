package runners

import java.io.{File, FileWriter}

import com.fasterxml.jackson.databind.{ObjectMapper, ObjectWriter, SequenceWriter, SerializationFeature}
import generator.JsonRandomGenerator

import scala.reflect.runtime.universe._

private[runners] trait JsonRandomGeneratorRunner {
  val jsonRandomGenerator: JsonRandomGenerator

  def run[A](topLevelObj: A, numOfRecords: Int)(implicit c: TypeTag[A]): Seq[A] = {

    def init(clazzName: String): SequenceWriter = {
      val file = new File(s"target/$clazzName.json")
      val fileWriter = new FileWriter(file, false)
      val mapper = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT)
      val seqWriter = mapper.writer[ObjectWriter]().writeValuesAsArray(fileWriter)
      seqWriter
    }

    val seqWriter = init(topLevelObj.getClass.getSimpleName)

    val res = (1 to numOfRecords).map { _ =>
      val jsonObj = jsonRandomGenerator.generate(topLevelObj)
      println(jsonObj)

      seqWriter.write(jsonObj)
      jsonObj
    }

    seqWriter.close()
    res
  }
}
