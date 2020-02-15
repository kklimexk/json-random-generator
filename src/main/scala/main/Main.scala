package main

import output.ComplexSchema
import utils.ReflectionUtils

object Main {
  def main(args: Array[String]): Unit = {
    val jsonObj = ReflectionUtils.evalMemberValues(new ComplexSchema())

    println(jsonObj)
    println(jsonObj.getBillingAddress)
  }
}
