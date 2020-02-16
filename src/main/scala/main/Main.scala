package main

import java.io.File

import com.fasterxml.jackson.databind.{ObjectMapper, SerializationFeature}
import generator.JsonRandomGenerator
import output.ComplexSchema

object Main {
  def main(args: Array[String]): Unit = {
    val jsonObj = JsonRandomGenerator.generate(new ComplexSchema())

    println(jsonObj)

    val mapper = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT)
    mapper.writeValue(new File(s"target/${jsonObj.getClass.getSimpleName}.json"), jsonObj)
    val jsonString = mapper.writeValueAsString(jsonObj)

    println(jsonString)
  }
}
