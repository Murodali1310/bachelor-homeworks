class Complex(val re: Double, val im: Double) {
  def +(z: Complex): Complex = new Complex(this.re + z.re, this.im + z.im)
  def -(z: Complex): Complex = new Complex(this.re - z.re, this.im - z.im)
  def *(z: Complex): Complex = {
    new Complex(this.re * z.re - this.im * z.im, this.re * z.im + this.im * z.re)
  }
  override def toString: String = {
    if (im < 0) s"$re - ${-im}i"
    else s"$re + ${im}i"
  }
}

object ComplexOperation {
  def readComplexNumberParser(): Complex = {
    val in = scala.io.StdIn.readLine()
    val parts = in.split("[+-]").map(_.trim)
    val re = parts(0).toDouble
    val im = if (in.contains("+")) parts(1).dropRight(1).toDouble else -parts(1).dropRight(1).toDouble
    new Complex(re, im)
  }
  
  def main(args: Array[String]): Unit = {
    var flag = true
    while (flag) {
      println("stdin z1, z2 (in the format a + bi):")
      val z1 = readComplexNumberParser()
      val z2 = readComplexNumberParser()
      println("z1 (+, -, *) z2 | op := ")
      val operation = scala.io.StdIn.readLine()
      val res: Complex = operation match {
        case "+" => z1 + z2
        case "-" => z1 - z2
        case "*" => z1 * z2
      }
      println(s"$res")
      println(res.re)
      println(s"res: Im = ")
      println(res.im)
      println("Continue ? (Y/N)")
      val choice: String = scala.io.StdIn.readLine()
      flag = choice.toUpperCase == "Y"
    }
  }
}

