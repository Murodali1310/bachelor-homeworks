package homework.task2

sealed trait UserType

object UserType {
  case object Dummy extends UserType

  case object AlmostCustomer extends UserType

  case object Customer extends UserType
}
