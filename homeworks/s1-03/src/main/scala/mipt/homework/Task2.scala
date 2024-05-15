package mipt.homework

/**
  * Задания в этом файле необходимо решать используя иммутабельные коллекции,
  * т.е. scala.collection._ и scala.collection.immutable._
  *
  * Любые методы из стандартной библиотеки в этом файле использовать можно.
  */
object Task2 {

  /**
    * Реализуйте метод removeMostFrequent.
    * В списке чисел нужно найти число с самым большим числом повторений, и вернуть новый список без этого числа
    * Если есть несколько разных чисел с одинаковой (максимальной) частотой, то удалить их все
    */
  def removeMostFrequent(numbers: Seq[Int]): Seq[Int] = {
    val frequencies             = numbers.groupBy(identity).map { case (num, numSeq) => (num, numSeq.size) }
    val maxFrequency            = if (frequencies.nonEmpty) frequencies.maxBy(_._2)._2 else 0
    val numbersWithMostFrequent = frequencies.toSeq.filter(_._2 == maxFrequency).map(_._1)
    numbers.filterNot(numbersWithMostFrequent.contains)
  }

  /**
    * Реализуйте метод smoothNumbers.
    * Для каждого элемента списка, нужно заменить его на среднее арифметическое этого элемента и двух соседних
    * Если какого-то из соседних элементов нет, то среднее необходимо считать не по 3, а по 2 или 1 значению.
    */
  def smoothNumbers(numbers: Seq[Int]): Seq[Double] = {
    numbers match {
      case Nil                 => Nil
      case head :: Nil         => Seq(head.toDouble)
      case head :: tail :: Nil => Seq((head + tail) / 2.0, (head + tail) / 2.0)
      case head :: tail =>
        val withNeighbors = numbers.sliding(3).map((subSeq: Seq[Int]) => subSeq.sum.toDouble / subSeq.size)
        val first         = (head + tail.head.toDouble) / 2
        val last          = (tail.init.last + tail.last).toDouble / 2
        Seq(first) ++ withNeighbors ++ Seq(last)
    }
  }

  case class User(lastName: String, firstName: String, middleName: String, age: Int)

  /**
    * Реализуйте метод sortUsers.
    * Есть список людей (фамилия, имя, отчество, возраст)
    * Нужно отсортировать его в следующем порядке: фамилия (лекс) -> возраст (по убыванию) -> имя (лекс) -> отчество (лекс)
    */
  def sortUsers(users: Seq[User]): Seq[User] = {
    users.sortBy(user => (user.lastName, -user.age, user.firstName, user.middleName))
  }

}
