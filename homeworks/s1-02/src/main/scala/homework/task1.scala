package homework

import utils.Homeworks._

object task1 {

  trait Building

  private case object PremiumHouse extends Building

  private case object RegularHouse extends Building

  case class House(houseType: Building, floors: Int, length: Double, width: Double, height: Double) {
    require(floors > 0 && length > 0 && width > 0 && height > 0, new IllegalArgumentException("Incorrect data"))

    def countParquetCost(): Double = {
      houseType match {
        case PremiumHouse if floors < 5  => math.pow(3, floors) * (length + width + height)
        case PremiumHouse if floors >= 5 => math.pow(2, floors) * (length + width + height)
        case RegularHouse                => length * width * height + floors * 10000
      }
    }
  }
}
