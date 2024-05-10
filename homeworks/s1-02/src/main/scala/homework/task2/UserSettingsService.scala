package homework.task2

trait UserSettingsService {
  def getPasswordSettings(userId: UserId): Option[PasswordSettings]
}

object UserSettingsService {
  def apply(userService: UserService): UserSettingsService = new UserSettingsService {
    def productType(phone: UserPhone): Int = {
      userService.findUserProducts(phone) match {
        case Some(product) =>
          product match {
            case UserProducts.LightProducts => 0
            case _                          => 1
          }
        case None => 1
      }
    }

    override def getPasswordSettings(userId: UserId): Option[PasswordSettings] = {
      val user: Option[User] = userService.findUser(userId)
      user match {
        case Some(User(_, UserType.Dummy, _)) =>
          Option(PasswordSettings(allowed = false, set = false))

        case Some(User(_, UserType.Customer, _)) =>
          val passwordInfo: PasswordInfo = userService.getPasswordInfo(userId)
          Option(PasswordSettings(allowed = true, set = passwordInfo.sent || !passwordInfo.temporary))

        case Some(User(_, UserType.AlmostCustomer, _)) =>
          user.get.phone match {
            case Some(phone) =>
              val passwordInfo: PasswordInfo = userService.getPasswordInfo(userId)
              productType(phone) match {
                case 0 =>
                  val value: Boolean = !passwordInfo.temporary || passwordInfo.sent
                  Some(PasswordSettings(allowed = value, set = value))
                case _ => Some(PasswordSettings(allowed = true, set = passwordInfo.sent || !passwordInfo.temporary))
              }
            case _ => None
          }
        case _ => None
      }
    }
  }
}
