package object utils {
  def executionTime[R](block: => R): R = {
    val t0 = System.currentTimeMillis() / 1000.0
    val result = block
    val t1 = System.currentTimeMillis() / 1000.0
    println(s"Records generated in: ${t1 - t0} s")
    result
  }
}
