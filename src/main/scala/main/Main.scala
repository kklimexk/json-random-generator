package main

import generator.JsonRandomGenerator
import output.ComplexSchema

object Main {
  def main(args: Array[String]): Unit = {
    val jsonObj = JsonRandomGenerator.generate(new ComplexSchema())

    println(jsonObj)
  }
}
