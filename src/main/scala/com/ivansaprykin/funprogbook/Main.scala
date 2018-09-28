package com.ivansaprykin.scalatesttask

import scala.io.StdIn


/*
Необходимо получить список Пицц с уникальной конфигурацией
Получать список булем в 4 этапа:
1-й в списке только одна пицца, состоящая из набора самых маленьких кусочков - начальная пицца
2-й постепенно добавляем в список новые пиццы, каждый раз сокращаем кол-во самых маленьких кусочков
    - объединяя их в бОльшие (размером 1/4)
3-й сейчас пицца состоит только из кусочков по 1/4 и 1/8, на этом шаге добавляем новую пиццу в список
    - с одним куском на 1/2 и кучей самых маленьких (по 1/8)
    повторяем 2-й шаг (получаем комбинации 1/2 + 1/4 + 1/8)
4-й добавляем в список пиццу с куском 8/8 и на оставшиеся места - куча самых маленьких кусков
    повторяем 2-й шаг (получаем комбинации 1. +  1/2 + 1/4 + 1/8)

 */


object Main extends App {

  // TODO: добавить функциональность проверки ввода на соответствие условию задачи
  def getXNumber(): Int = StdIn.readInt() % 7 + 7

  val x: Int = getXNumber()
  val pizzaParser: PizzaParser = new PizzaParser()

  println(x + "/8 pizza:")
  pizzaParser.findAllPizzaVariants(x).foreach(println)

}

/** Программа должна считывать дату рождения в формате DDMMYY */
class UserInputBirthDayChecker {
  // TODO: scala way - Option or Future
  def isCorrect(input: Int): Boolean = ???
}


case class Pizza(sizeInSmallestPieces: Int, pieces: List[PizzaPiece]) {

  private def getNumberOfPizzaPiecesBySize(pizzaPieceSize: Int): Int = pieces.foldLeft(0) { (acc, pizzaPiece) =>
    pizzaPiece.size match {
      case `pizzaPieceSize` => acc + pizzaPiece.numberOfPieces
      case _ => acc
    }
  }

  def getNumberOfSmallestPizzaPieces: Int = getNumberOfPizzaPiecesBySize(PizzaPiece.oneEighthOfaPizza)
  def getNumberOfOneHalfPizzaPieces: Int = getNumberOfPizzaPiecesBySize(PizzaPiece.halfOfaPizza)
  def getNumberOfFullPizzaPieces: Int = getNumberOfPizzaPiecesBySize(PizzaPiece.fullPizza)

  override def toString: String = pieces.mkString("[", ", ", "]")
}

object Pizza {
  // overloading constructor for creating pizza with only smallest pieces
  // can't use default parameters
  // because need to use constructor parameter's value to create first element of the list
  // (and that list is parameter for second argument needed to construct Pizza)
  def apply(sizeInSmallestPieces: Int): Pizza = new Pizza(sizeInSmallestPieces, List(
    PizzaPiece(PizzaPiece.oneEighthOfaPizza, sizeInSmallestPieces),
    PizzaPiece(PizzaPiece.oneFourthOfaPizza, 0),
    PizzaPiece(PizzaPiece.halfOfaPizza, 0),
    PizzaPiece(PizzaPiece.fullPizza, 0)
  ))
}

case class PizzaPiece(size: Int, numberOfPieces: Int) { // size: 1/8 пиццы это 1 , 1/4 это 2 , 1/2 это 4 , 1 это 8
  override def toString: String = List.fill(numberOfPieces)("1/" + 8 / size).mkString(", ")
}

object PizzaPiece {
  val oneEighthOfaPizza: Int = 1
  val oneFourthOfaPizza: Int = 2
  val halfOfaPizza: Int = 4
  val fullPizza: Int = 8
}

class PizzaParser {

  def findAllPizzaVariants(pizzaSizeInSmallestPieces: Int): List[Pizza] = {
    val initialPizza: Pizza = Pizza(pizzaSizeInSmallestPieces)
    loop(List(initialPizza))
  }

  private def loop(listOfPizzaVariants: List[Pizza]): List[Pizza] = {
    // до тех пор пока можно добавить еще вариант пиццы (путем или сокращения нескольких маленьких кусков в большие
    // или добавления очередного большого куска и на оставшиеся большого количества самых маленьких кусков)
    //    - добавляю вариант в аккумулятор,
    // если возможности добавить нет, значит все варианты найдены

    // ищу в пицце наименьший кусок, смотрю сколько таких кусков в ней есть и можно ли "сократить" 2 меньших куска в 1 б`ольший кусок,
    // такми образом получив новую пиццу, добавляю ее в список, и дальше уже на ее основе создаю новые

    val latestPizza: Pizza = listOfPizzaVariants.head

    def canAddMediumSizePiece: Boolean = latestPizza.getNumberOfSmallestPizzaPieces >= 2

    def addPizzaToListNewMediumPiece(): List[Pizza] = {
      def addNewPieceByReducing(currentPiece: PizzaPiece): PizzaPiece =
      (currentPiece.size, currentPiece.numberOfPieces) match { // TODO: exception handling
        case (PizzaPiece.oneEighthOfaPizza, num) => PizzaPiece(PizzaPiece.oneEighthOfaPizza, num - 2)
        case (PizzaPiece.oneFourthOfaPizza, num) => PizzaPiece(PizzaPiece.oneFourthOfaPizza, num + 1)
        case (PizzaPiece.halfOfaPizza, num) => PizzaPiece(PizzaPiece.halfOfaPizza, num)
        case (PizzaPiece.fullPizza, num) => PizzaPiece(PizzaPiece.fullPizza, num)
      }

      listOfPizzaVariants.::(Pizza(latestPizza.sizeInSmallestPieces, latestPizza.pieces.map(p => addNewPieceByReducing(p))))
    }

    def canAddHalfSizePiece: Boolean = {
      // проверяем можно ли добавить еще кусок размером 1/2  при этом проверяем, что текущее количество
      // кусков по 1 и 1/2 позволяют это сделать не выходя за границы пиццы по маленьким кусочкам (исходное число Х)

      val numberOccupiedByHalfPizzaPieces: Int = latestPizza.getNumberOfOneHalfPizzaPieces * PizzaPiece.halfOfaPizza
      val numberOccupiedByFullPizzaPieces: Int = latestPizza.getNumberOfFullPizzaPieces * PizzaPiece.fullPizza
      val numberOfFreeSmallestPieces: Int = latestPizza.sizeInSmallestPieces - numberOccupiedByHalfPizzaPieces - numberOccupiedByFullPizzaPieces

      numberOfFreeSmallestPieces >= 4
    }

    def addPizzaToListNewHalfSizePiece(): List[Pizza] = { // создаем нвую пиццу, в которой кол-во кусочков размером 1/2 (4) увеличено на 1,

      val numberOccupiedByHalfPizzaPieces: Int = (latestPizza.getNumberOfOneHalfPizzaPieces + 1) * PizzaPiece.halfOfaPizza
      val numberOccupiedByFullPizzaPieces: Int = latestPizza.getNumberOfFullPizzaPieces * PizzaPiece.fullPizza
      val numberOfSmallestPiecesToAdd: Int = latestPizza.sizeInSmallestPieces - numberOccupiedByHalfPizzaPieces - numberOccupiedByFullPizzaPieces

      def addOneHalfPiece(currentPiece: PizzaPiece): PizzaPiece =
      (currentPiece.size, currentPiece.numberOfPieces) match { // TODO: exception handling
        case (PizzaPiece.oneEighthOfaPizza, _) => PizzaPiece(PizzaPiece.oneEighthOfaPizza, numberOfSmallestPiecesToAdd) // кол-во маленьких кусочков = x - кол-во кусков по 1/2 * (4) и 1. * (8)
        case (PizzaPiece.oneFourthOfaPizza, _) => PizzaPiece(PizzaPiece.oneFourthOfaPizza, 0) // кол-во кусочков размером 1/4 (2) равно 0
        case (PizzaPiece.halfOfaPizza, num) => PizzaPiece(PizzaPiece.halfOfaPizza, num + 1) // + 1 кусок весом 1/2 (4), т.к. учитываем тот, котороый собираемся добавить
        case (PizzaPiece.fullPizza, num) => PizzaPiece(PizzaPiece.fullPizza, num) // кол-во кусочков размером 1. (8) не изменяется
      }

      listOfPizzaVariants.::(Pizza(latestPizza.sizeInSmallestPieces, latestPizza.pieces.map(p => addOneHalfPiece(p))))
    }

    // это происходит, если максимальное количество кусков в пол-пиццы уже есть (мах 1/2 уже достигнут), т.е. x - number * 4  < 4)
    // по условию может быть только один кусок пиццы размером 1. (8) - условием его добавления будет:
    // отсутствие такого куска, невозможность добавить еще кусков размером поменьше (1/2)
    // т.к. нельзя "раньше времени" (пока не перебраны все варианты пицц из меньших кусков) добавить самый большой кусок
    def canAddFullPizzaPiece: Boolean = {
      val latestPizzaContainsNoFullPizzaPiece = latestPizza.getNumberOfFullPizzaPieces == 0
      val cantAddMoreHalfPizzaPieces = true // redundant - логика использования этого метода подразумевает,
      // что он вызывается только после того, когда получены все варианты с кусками пиццы размером 1/2
      latestPizzaContainsNoFullPizzaPiece && cantAddMoreHalfPizzaPieces
    }

    def addPizzaToListNewFullSizePiece(): List[Pizza] = {

      val numberOfSmallestPiecesToAdd: Int = latestPizza.sizeInSmallestPieces - PizzaPiece.fullPizza

      def addFullPizzaPiece(currentPiece: PizzaPiece): PizzaPiece =
      (currentPiece.size, currentPiece.numberOfPieces) match { // TODO: exception handling
        case (PizzaPiece.oneEighthOfaPizza, _) => PizzaPiece(PizzaPiece.oneEighthOfaPizza, numberOfSmallestPiecesToAdd)
        case (PizzaPiece.oneFourthOfaPizza, _) => PizzaPiece(PizzaPiece.oneFourthOfaPizza, 0)
        case (PizzaPiece.halfOfaPizza, _) => PizzaPiece(PizzaPiece.halfOfaPizza, 0)
        case (PizzaPiece.fullPizza, num) => PizzaPiece(PizzaPiece.fullPizza, num + 1)
      }

      listOfPizzaVariants.::(Pizza(latestPizza.sizeInSmallestPieces, latestPizza.pieces.map(p => addFullPizzaPiece(p))))
    }

    if (canAddMediumSizePiece) loop(addPizzaToListNewMediumPiece())
    else if (canAddHalfSizePiece) loop(addPizzaToListNewHalfSizePiece())
    else if (canAddFullPizzaPiece) loop(addPizzaToListNewFullSizePiece())
    else listOfPizzaVariants
  }

}