package homework

import utils.Homeworks._

object task3 {

  sealed trait Ternary

  object Ternary {
    case object Yes extends Ternary

    case object No extends Ternary

    case object Maybe extends Ternary
  }

  /**
    * Класс определяет эквивалентность типов
    * Для этого он получает функции прямого и обратного преобразований.
    * Необходимо передавать такие функции, что бы соблюдались правила:
    *
    * Для любого x, to(from(x)) = x
    * Для любого y, from(to(y)) = y
    *
    * @param to   функция прямого преобразования
    * @param from функция обратного преобразования
    */
  class Equivalent[A, B](val to: A => B)(val from: B => A)

  object Equivalent {
    def apply[A, B](to: A => B)(from: B => A) =
      new Equivalent[A, B](to)(from)
  }

  /*
   * Решите следующие задания.
   * Вам необходимо реализовать функции прямого и обратного преобразований так,
   * что бы для инициализированного ими Equivalent выполнялись условия эквивалентности
   */

  task"определите эквивалентность экспоненты в булевый тип и произведения 3 ^ 2 = 3 * 3 "

  def boolToThree: Equivalent[Boolean => Ternary, (Ternary, Ternary)] = Equivalent { (forward: Boolean => Ternary) =>
    (forward(true), forward(false))
  } { (backward: (Ternary, Ternary)) =>
    {
      case true  => backward._1
      case false => backward._2
    }
  }

  task"определите эквивалентность экспоненты в тернарный тип и произведения 2 ^ 3 = 2 * 2 * 2"

  def threeToBool: Equivalent[Ternary => Boolean, (Boolean, Boolean, Boolean)] = Equivalent {
    (forward: Ternary => Boolean) => (forward(Ternary.Yes), forward(Ternary.No), forward(Ternary.Maybe))
  } { (backward: (Boolean, Boolean, Boolean)) =>
    {
      case Ternary.Yes   => backward._1
      case Ternary.No    => backward._2
      case Ternary.Maybe => backward._3
    }
  }

  task"определите эквивалентность двух экпонент высшего порядка булевых типов (2 ^ 2) ^ 2 = 2 ^ (2 ^ 2)"

  def boolToBoolToBool: Equivalent[Boolean => Boolean => Boolean, (Boolean => Boolean) => Boolean] = Equivalent {
    (forward: Boolean => Boolean => Boolean) => h: (Boolean => Boolean) => forward(h(true))(h(false))
  } { (backward: (Boolean => Boolean) => Boolean) => a: Boolean => b: Boolean => backward(c => if (c) a else b) }

  task"докажите тривиальное качество терминального типа Unit 1 ^ A = 1"

  def AToUnit[A]: Equivalent[A => Unit, Unit] = Equivalent { (forward: A => Unit) => () } {
    (backward: Unit) => (_: A) => ()
  }

  task"докажите тривиальность экспоненты с единичным показателем A ^ 1 = A"

  def unitToA[A]: Equivalent[Unit => A, A] = Equivalent { (forward: Unit => A) => forward(()) } {
    (backward: A) => (_: Unit) => backward
  }

  task"докажите фундаментальное качество паттерн-матчинга C ^ (A + B) = C ^ A * C ^ B"

  def patternMatching[A, B, C]: Equivalent[(A Either B) => C, (A => C, B => C)] = Equivalent {
    (forward: (A Either B) => C) => ((left: A) => forward(Left(left)), (right: B) => forward(Right(right)))
  } { (backward: (A => C, B => C)) =>
    {
      case Left(left)   => backward._1(left)
      case Right(right) => backward._2(right)
    }
  }
}
