package homework

import utils.Homeworks._

object task3 {

  sealed trait Ternary
  object Ternary {
    case object Yes   extends Ternary
    case object No    extends Ternary
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
   * @param to функция прямого преобразования
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

  def boolToThree: Equivalent[Boolean => Ternary, (Ternary, Ternary)] =
    Equivalent {
      (f: Boolean => Ternary) => (f(true), f(false))
    } {
      (pair: (Ternary, Ternary)) => {
        case true => pair._1
        case false => pair._2
      }
    }

  task"определите эквивалентность экспоненты в тернарный тип и произведения 2 ^ 3 = 2 * 2 * 2"
  def threeToBool: Equivalent[Ternary => Boolean, (Boolean, Boolean, Boolean)] =
    Equivalent {
      (f: Ternary => Boolean) => (f(Ternary.Yes), f(Ternary.No), f(Ternary.Maybe))
    } {
      (tuple: (Boolean, Boolean, Boolean)) => {
        case Ternary.Yes => tuple._1
        case Ternary.No => tuple._2
        case Ternary.Maybe => tuple._3
      }
    }
  task"определите эквивалентность двух экпонент высшего порядка булевых типов (2 ^ 2) ^ 2 = 2 ^ (2 ^ 2)"

  def boolToBoolToBool: Equivalent[Boolean => Boolean => Boolean, (Boolean => Boolean) => Boolean] =
    Equivalent {
      (f: Boolean => Boolean => Boolean) => g: (Boolean => Boolean) => f(g(true))(g(false))
    } {
      (f: (Boolean => Boolean) => Boolean) => a: Boolean => b: Boolean => f(c => if(c) a else b)
    }

  task"докажите тривиальное качество терминального типа Unit 1 ^ A = 1"
  def AToUnit[A]: Equivalent[A => Unit, Unit] =
    Equivalent {
      (_: A => Unit) => ()
    } {
      (_: Unit) => (_: A) => ()
    }

  task"докажите тривиальность экспоненты с единичным показателем A ^ 1 = A"

  def unitToA[A]: Equivalent[Unit => A, A] =
    Equivalent {
      (f: Unit => A) => f(())
    } {
      (a: A) => (_: Unit) => a
    }
  task"докажите фундаментальное качество паттерн-матчинга C ^ (A + B) = C ^ A * C ^ B"

  def patternMatching[A, B, C]: Equivalent[(A Either B) => C, (A => C, B => C)] =
    Equivalent(
      (f: Either[A, B] => C) => ((a: A) => f(Left(a)), (b: B) => f(Right(b)))
    )(
      (g: (A => C, B => C)) =>
      {
        case Left(a) => g._1(a)
        case Right(b) => g._2(b)
      }
    )
}