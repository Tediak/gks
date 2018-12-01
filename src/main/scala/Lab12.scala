import gks._
import gks.Group._
import gks.Util._

import scala.language.postfixOps

object Lab12 {
  OperationList.foreach { lst =>
    lst.foreach(op => print(op + " "))
    println
  }

  "unique operations".task()

  val UniqueOperations: List[String] = OperationList.flatten.toSet.toSeq.sortWith(_ < _).toList

  UniqueOperations.foreach(op => print(op + " "))

  println("\nCount of unique operations: " + UniqueOperations.size)
  val FirstMatrix: List[List[Int]] =
    OperationList.map { lst =>
      UniqueOperations.map { u =>
        if (lst.contains(u)) 1
        else 0
      }
    }

  "first matrix".task()

  println(UniqueOperations.mkString(" |") + " |")
  FirstMatrix foreach { lst =>
    lst foreach (i => print(" " + i + " |"))
    println
  }

  "second matrix".task()

  val SecondMatrix: Seq[List[Int]] = OperationList.map { lst =>
    val result = for (lst2 <- OperationList)
      yield (lst ++ lst2).groupBy(x => x).filter(_._2.size < 2).keys.size
    result.map(UniqueOperations.size - _).map(el => if (el == UniqueOperations.size) 0 else el)
  }

  SecondMatrix.foreach { lst =>
    lst.foreach(el => print(el + {
      if (el > 9) " " else "  "
    } + "|"))
    println
  }

  "unique numbers".task()

  val UniqueNumbers: Seq[Int] = SecondMatrix.flatten.sortBy(x => x).reverse.distinct.filter(_ != 0)

  println(UniqueNumbers.mkString(", "))

  "groups".task()

  def findGroup(num: Int, matrix: List[List[Int]], allGroups: List[Group]): List[Group] = {
    val coordinates = matrix
      .map(_
        .zipWithIndex
        .filter(_._1 == num)
        .map(_._2))
      .zipWithIndex
      .map { x => x._1.map((_, x._2)) }
      .filter(_.nonEmpty)
      .flatten
      .filterNot(x =>
        allGroups.getDetails.distinct.contains(x._1) ||
          allGroups.getDetails.toSet.contains(x._2))

    def makeGroup(coords: List[(Int, Int)], groups: List[Group]): List[Group] = {
      coords match {
        case x :: xs =>
          val nums = groups.getDetails.toSet
          (nums.contains(x._1), nums.contains(x._2)) match {
            case (true, false) =>
              val newGroups =
                (groups
                  .map(_.details)
                  .filter(_.contains(x._1))
                  .map(x._2 :: _) ++
                  groups.map(_.details)
                    .filterNot(_.contains(x._1))).map (Group.ofDetails)
              makeGroup(xs, newGroups)
            case (false, true) =>
              val newGroups =
                (groups.map(_.details)
                  .filter(_.contains(x._2))
                  .map(x._1 :: _) ++
                  groups
                    .map(_.details)
                    .filterNot(_.contains(x._2))).map (Group.ofDetails)
              makeGroup (xs, newGroups)
            case (true, true) =>
              makeGroup(xs, groups)
            case (false, false) =>
              makeGroup(xs, Group.ofDetails(x._1 :: x._2 :: Nil) :: groups)
          }
        case Nil =>
          groups
      }
    }

    val groupsForThatNum = makeGroup(coordinates, Nil)
    val nums = allGroups.getDetails.toSet
    allGroups ++
      groupsForThatNum
        .map(_.details
          .filterNot(n =>
            nums.contains(n)))
        .filter(_.size > 1)
        .makeGroupFromDetails
  }

  def findAllGroups(nums: List[Int], matrix: List[List[Int]], groups: List[Group] = Nil): List[Group] = {
    nums match {
      case num :: left =>
        findAllGroups(left, matrix, groups ++ findGroup(num, matrix, groups))
      case Nil =>
        groups.toSet.toList
    }
  }

  val maybeAllGroups: List[Group] = findAllGroups(UniqueNumbers.toList, SecondMatrix.toList)

  val AllGroups: List[Group] = {
    maybeAllGroups ++
      UniqueOperations
        .indices
        .toList
        .filterNot(op =>
          maybeAllGroups
            .getDetails
            .contains(op))
        .map(List(_)).makeGroupFromDetails
  }


  AllGroups show()

  "sorted groups".task()

  val SortedGroups: List[Group] =
    AllGroups
      .sortWith(_.operations.size > _.operations.size)

  SortedGroups show()

  "optimized groups".task()

  def optimizeGroups(groups: List[Group]): List[Group] = {


    def optimizeEachGroup(mainGroup: Group,
                          restOfGroups: List[Group],
                          acc: List[Group] = Nil): List[Group] = {
      def needToMove(detail: Int): Boolean = OperationList(detail).forall(mainGroup.operations.contains)

      restOfGroups match {
        case Nil =>
          (mainGroup :: acc).filter(_.nonEmpty)
        case Group(det, _) :: rest =>
          val toMove: Group =
            Group(det.filter(needToMove))
          val notToMove: Group =
            Group(det.filterNot(needToMove))
          optimizeEachGroup(mainGroup + toMove, rest, notToMove :: acc)
      }
    }
    optimizeEachGroup(groups.head, groups.tail)
  }

  val OptimizedGroups: List[Group] = optimizeGroups(SortedGroups)

  OptimizedGroups.show()

}
