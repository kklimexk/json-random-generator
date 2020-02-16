package main

import output.{ComplexSchema, NumbersArr}
import utils.ReflectionUtils

object Main {
  def main(args: Array[String]): Unit = {
    val jsonObj = ReflectionUtils.evalMemberValues(new ComplexSchema())

    println(jsonObj)
  }
}
