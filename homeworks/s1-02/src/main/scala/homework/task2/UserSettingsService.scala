package homework.task2

trait UserSettingsService {
  def getPasswordSettings(userId: UserId): Option[PasswordSettings]
}

object UserSettingsService {

  def apply(userService: UserService): UserSettingsService = (userId: UserId) => {
    val passwordInfo  = userService.getPasswordInfo(userId)
    val isPasswordSet = passwordInfo.sent || !passwordInfo.temporary
    userService.findUser(userId) match {
      case Some(user) =>
        user.`type` match {
          case UserType.Dummy => Some(PasswordSettings(set = false, allowed = false))
          case UserType.Customer =>
            Some(PasswordSettings(allowed = true, set = isPasswordSet))
          case UserType.AlmostCustomer =>
            user.phone match {
              case Some(phone) =>
                userService.findUserProducts(phone) match {
                  case Some(UserProducts.HeavyProducts) => Some(PasswordSettings(allowed = true, set = isPasswordSet))
                  case Some(UserProducts.LightProducts) =>
                    Some(PasswordSettings(allowed = isPasswordSet, set = isPasswordSet))
                  case None => Some(PasswordSettings(allowed = true, set = isPasswordSet))
                }
              case _ => None
            }
        }
      case _ => None
    }
  }
}