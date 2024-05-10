package homework

import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class task2Spec extends AnyFunSpec with Matchers {
  import task2._

  describe("UserPhone") {
    def testCase(phone: String, isValid: Boolean)(implicit pos: Position): Unit =
      it(s"$phone isValid=$isValid") {
        UserPhone(phone).nonEmpty shouldBe isValid
      }

    testCase("+79152037922", isValid = true)
    testCase("+77152037922", isValid = false)
    testCase("+152037922", isValid = false)
    testCase("+7915203792222", isValid = false)
    testCase("79152037922", isValid = false)
  }

  describe("UserId") {
    def testCase(id: Long, isValid: Boolean)(implicit pos: Position): Unit =
      it(s"$id isValid=$isValid") {
        UserId(id).nonEmpty shouldBe isValid
      }

    testCase(2288, isValid = true)
    testCase(666, isValid = true)
    testCase(0, isValid = true)
    testCase(-1, isValid = false)
    testCase(-100, isValid = false)
    testCase(-1812912, isValid = false)
    testCase(-921379123, isValid = false)
  }

  describe("getPasswordSettings") {
    val phone = UserPhone("+79152281337").get

    val baseUser = User(
      id = UserId(0).get,
      `type` = UserType.Dummy,
      phone = None
    )

    testCase(
      description = "return none if user not found",
      user = None,
      expectedResult = None
    )

    testCase(
      description = "return set=false, allowed=false for Dummy",
      user = Some(baseUser.copy(`type` = UserType.Dummy)),
      expectedResult = Some(PasswordSettings(set = false, allowed = false))
    )

    testCase(
      description = "return set=true if password is not temporary, allowed=true for Customer",
      user = Some(baseUser.copy(`type` = UserType.Customer)),
      passwordInfo = PasswordInfo(temporary = false, sent = false),
      expectedResult = Some(PasswordSettings(set = true, allowed = true))
    )

    testCase(
      description = "return set=false if password is temporary and not sent, allowed=true for Customer",
      user = Some(baseUser.copy(`type` = UserType.Customer)),
      passwordInfo = PasswordInfo(temporary = true, sent = false),
      expectedResult = Some(PasswordSettings(set = false, allowed = true))
    )

    testCase(
      description = "return set=true if password is temporary and sent, allowed=true for Customer",
      user = Some(baseUser.copy(`type` = UserType.Customer)),
      passwordInfo = PasswordInfo(temporary = true, sent = true),
      expectedResult = Some(PasswordSettings(set = true, allowed = true))
    )

    testCase(
      description = "return None if almost customer, but has no phone",
      user = Some(baseUser.copy(`type` = UserType.AlmostCustomer, phone = None)),
      expectedResult = None
    )

    List(Some(UserProducts.HeavyProducts), None).foreach { products =>
      testCase(
        description =
          s"return allowed=true set=true if almost customer with phone and products=$products and password not temporary",
        user = Some(baseUser.copy(`type` = UserType.AlmostCustomer, phone = Some(phone))),
        products = products,
        passwordInfo = PasswordInfo(temporary = false, sent = false),
        expectedResult = Some(PasswordSettings(set = true, allowed = true))
      )

      testCase(
        description =
          s"return allowed=true set=true if almost customer with phone and products=$products and password temporary and sent",
        user = Some(baseUser.copy(`type` = UserType.AlmostCustomer, phone = Some(phone))),
        products = products,
        passwordInfo = PasswordInfo(temporary = true, sent = true),
        expectedResult = Some(PasswordSettings(set = true, allowed = true))
      )

      testCase(
        description =
          s"return allowed=false set=false if almost customer with phone and products=$products and password temporary and not sent",
        user = Some(baseUser.copy(`type` = UserType.AlmostCustomer, phone = Some(phone))),
        products = products,
        passwordInfo = PasswordInfo(temporary = true, sent = false),
        expectedResult = Some(PasswordSettings(set = false, allowed = true))
      )

    }

    testCase(
      description =
        s"return allowed=true set=true if almost customer with phone and products is light and password not temporary",
      user = Some(baseUser.copy(`type` = UserType.AlmostCustomer, phone = Some(phone))),
      products = Some(UserProducts.LightProducts),
      passwordInfo = PasswordInfo(temporary = false, sent = false),
      expectedResult = Some(PasswordSettings(set = true, allowed = true))
    )

    testCase(
      description =
        s"return allowed=true set=true if almost customer with phone and products is light and password temporary and sent",
      user = Some(baseUser.copy(`type` = UserType.AlmostCustomer, phone = Some(phone))),
      products = Some(UserProducts.LightProducts),
      passwordInfo = PasswordInfo(temporary = true, sent = true),
      expectedResult = Some(PasswordSettings(set = true, allowed = true))
    )

    testCase(
      description =
        s"return allowed=false set=false if almost customer with phone and products is light and password temporary and not sent",
      user = Some(baseUser.copy(`type` = UserType.AlmostCustomer, phone = Some(phone))),
      products = Some(UserProducts.LightProducts),
      passwordInfo = PasswordInfo(temporary = true, sent = false),
      expectedResult = Some(PasswordSettings(set = false, allowed = false))
    )

    def testCase(
        description: String,
        user: Option[User] = None,
        products: Option[UserProducts] = None,
        passwordInfo: PasswordInfo = PasswordInfo(sent = false, temporary = true),
        expectedResult: Option[PasswordSettings]
    )(implicit pos: Position): Unit = it(description) {
      val userService = new UserService {
        override def findUser(userId: UserId) = user

        override def findUserProducts(phone: UserPhone) = products

        override def getPasswordInfo(userId: UserId) = passwordInfo
      }

      UserSettingsService(userService).getPasswordSettings(baseUser.id) shouldBe expectedResult

    }
  }

}
