package homework.task2

sealed trait UserProducts

object UserProducts {
  case object LightProducts extends UserProducts

  case object HeavyProducts extends UserProducts
}
