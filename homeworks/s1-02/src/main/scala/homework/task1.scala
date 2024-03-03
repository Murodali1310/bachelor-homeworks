// task1.scala

package homework

import utils.Homeworks._

object task1 {

  trait Building

  sealed trait HouseType
  case object Premium extends HouseType
  case object Economy extends HouseType

  case class House(houseType: HouseType, floors: Int, length: Double, width: Double, height: Double) {
    require(floors > 0 && length > 0 && width > 0 && height > 0, "Invalid house parameters")

    def calculateParquetCost: Double = houseType match {
      case Premium if floors < 5 => Math.pow(3, floors) * (length + width + height)
      case Premium => Math.pow(2, floors) * (length + width + height)
      case Economy => length * width * height + floors * 10000
    }
  }

  def main(args: Array[String]): Unit = {
    // Создаем экземпляры домов
    val premiumHouse = House(Premium, 4, 10, 5, 3)
    val economyHouse = House(Economy, 2, 8, 6, 2)

    // Рассчитываем стоимость паркета для каждого дома
    val premiumParquetCost = premiumHouse.calculateParquetCost
    val economyParquetCost = economyHouse.calculateParquetCost

    // Выводим результаты
    println(s"Premium house parquet cost: $premiumParquetCost")
    println(s"Economy house parquet cost: $economyParquetCost")

  }
}
