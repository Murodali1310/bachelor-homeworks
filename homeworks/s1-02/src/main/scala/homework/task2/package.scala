package homework

package object task2 {
  type UserId = Long

  object UserId {
    def apply(long: Long): Option[UserId] = {
      long match {
        case n if n >= 0 => Option(long)
        case _           => None
      }
    }
  }

  type UserPhone = String

  object UserPhone {
    def apply(s: String): Option[UserPhone] = {
      s match {
        case m if (m.startsWith("+79") && m.length == 12) => Option(s)
        case _                                            => None
      }
    }
  }
}
