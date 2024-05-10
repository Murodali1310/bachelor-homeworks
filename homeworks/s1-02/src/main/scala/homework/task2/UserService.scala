package homework.task2

trait UserService {
  def findUser(userId: UserId): Option[User]

  def findUserProducts(phone: UserPhone): Option[UserProducts]

  def getPasswordInfo(userId: UserId): PasswordInfo
}
