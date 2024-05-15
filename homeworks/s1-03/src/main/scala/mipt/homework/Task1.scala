package mipt.homework

import scala.annotation.tailrec

object Task1 extends App {

  /** Реализуйте метод take, который возвращает n первых элементов исходного
    * списка. Для реализации используйте хвостовую рекурсию, методы foldLeft и
    * reverse из класса списка. Метод должен работать за линейное время
    */
  def take[A](l: MyList[A], n: Int): MyList[A] = {
    @tailrec
    def takeRec[B](l: MyList[B], n: Int, acc: MyList[B]): MyList[B] = {
      if (n > 0) l match {
        case MyNil              => acc
        case MyCons(head, tail) => takeRec(tail, n - 1, MyCons(head, acc))
      }
      else acc
    }
    takeRec(l, n, MyNil).reverse
  }

  /** Реализуйте метод takeRight, который возвращает n последних элементов
    * исходного списка. Для реализации используйте хвостовую рекурсию, методы
    * foldLeft и reverse из класса списка. Метод должен работать за линейное
    * время
    */
  def takeRight[A](l: MyList[A], n: Int): MyList[A] = take(l.reverse, n).reverse

  /** Реализуйте метод takeWhile, который идет по переданному списку с головы и
    * собирает элементы в новый список до тех пор, пока предикат выполняется.
    * Для реализации используйте методы foldLeft и reverse из класса списка.
    * Метод должен работать за линейное время.
    */
  def takeWhile[A](l: MyList[A], predicate: A => Boolean): MyList[A] = {
    @tailrec
    def takeWhileRec[B](
        l: MyList[B],
        predicate: B => Boolean,
        acc: MyList[B]
    ): MyList[B] =
      l match {
        case MyNil => acc
        case MyCons(head, tail) =>
          if (predicate(head)) takeWhileRec(tail, predicate, MyCons(head, acc))
          else acc
      }

    takeWhileRec(l, predicate, MyNil).reverse
  }

  /** Реализуйте метод dropWhile, который отбрасывает элементы исходного списка
    * начиная с головы, пока они удовлетворяют переданному предикату.
    * Использовать для реализации хвостовую рекурсию, методы foldLeft и reverse
    * из класса списка. Метод должен работать за линейное время
    */
  @tailrec
  def dropWhile[A](l: MyList[A], predicate: A => Boolean): MyList[A] = l match {
    case MyNil => MyNil
    case MyCons(head, tail) =>
      if (predicate(head)) dropWhile(tail, predicate) else l
  }

  /** Реализуйте метод zip, который из двух списков делает один, объединяя их
    * элементы попарно в анонимный кортеж (tuple). Итоговый список должен иметь
    * длину, равную длине более короткого списка, элементы из хвоста длинного
    * списка игнорируются. Для реализации использовать хвостовую рекурсию,
    * методы foldLeft и reverse из класса списка. Метод должен работать за
    * линейное время
    */
  def zip[A, B](l1: MyList[A], l2: MyList[B]): MyList[(A, B)] = {
    @tailrec
    def zipRec[C, D](
        l1: MyList[C],
        l2: MyList[D],
        acc: MyList[(C, D)]
    ): MyList[(C, D)] =
      l1 match {
        case MyCons(head1, tail1) =>
          l2 match {
            case MyCons(head2, tail2) =>
              zipRec(tail1, tail2, MyCons((head1, head2), acc))
            case MyNil => acc
          }
        case MyNil => MyNil
      }
    zipRec(l1, l2, MyNil).reverse
  }

}
