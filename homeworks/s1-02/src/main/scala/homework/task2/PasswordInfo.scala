package homework.task2

/**
  *
  * @param sent      Пароль отправлен пользователю в SMS
  * @param temporary Пароль временный
  */
case class PasswordInfo(sent: Boolean, temporary: Boolean)
