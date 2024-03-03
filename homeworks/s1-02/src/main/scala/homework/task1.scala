// task1.scala

package homework

import utils.Homeworks._

object task1 {

  trait Building

  object UserPhone {
    def apply(phone: String): Option[String] = {
      val regex = raw"\+79\d{9}".r
      if (regex.matches(phone)) Some(phone) else None
    }
  }

  def main(args: Array[String]): Unit = {
    val validRussianPhone = "+79123456789"
    val invalidRussianPhone = "+78123456789"
    val invalidPhone = "1234567890"

    val validRussianPhoneResult = UserPhone(validRussianPhone)
    val invalidRussianPhoneResult = UserPhone(invalidRussianPhone)
    val invalidPhoneResult = UserPhone(invalidPhone)

    println(s"Valid Russian phone: $validRussianPhone -> $validRussianPhoneResult")
    println(s"Invalid Russian phone: $invalidRussianPhone -> $invalidRussianPhoneResult")
    println(s"Invalid phone: $invalidPhone -> $invalidPhoneResult")

    // Проверка на тестовые случаи
    assert(UserPhone("+79123456789") == Some("+79123456789"))
    assert(UserPhone("+78123456789") == None)
    assert(UserPhone("1234567890") == None)
  }
}
