package main

import com.example.types.AddressSchema
import utils.ReflectionUtils

object Main {
  def main(args: Array[String]): Unit = {
    ReflectionUtils.evalMemberValues(new AddressSchema)
  }
}
