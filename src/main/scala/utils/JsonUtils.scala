package utils

import java.io.{File, FileWriter}

import com.fasterxml.jackson.databind.{ObjectMapper, ObjectWriter, SequenceWriter, SerializationFeature}

object JsonUtils {

  object Implicits {
    implicit class SaveGeneratedObjects(objects: Seq[_]) {
      def save(jsonTargetPath: String): Unit = {
        def init(): SequenceWriter = {
          val file = new File(jsonTargetPath)
          val fileWriter = new FileWriter(file, false)
          val mapper = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT)
          val seqWriter = mapper.writer[ObjectWriter]().writeValuesAsArray(fileWriter)
          seqWriter
        }

        val seqWriter = init()

        objects.foreach(seqWriter.write)

        seqWriter.close()
      }
    }
  }
}
