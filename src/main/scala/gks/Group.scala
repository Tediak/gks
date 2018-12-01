package gks

import scalax.collection.Graph
import scalax.collection.edge.Implicits._
import scalax.collection.edge.LDiEdge
import Util._


case class Group(details: List[Int], operations: List[String]) {
  def +(that: Group): Group =
    Group(this.details ::: that.details)

  val isEmpty: Boolean = details.isEmpty && operations.isEmpty

  val nonEmpty: Boolean = details.nonEmpty && operations.nonEmpty

  override def toString: String = s"-group [ ${details.sortWith(_ < _).mkString(" ")} ] ( " +
    s"${operations.sortWith(_ < _).mkString(" ")} )"

  def getGraph: Graph[String, LDiEdge] = {
    def makePairs(lst: List[String]): List[(String, String)] =
      lst match {
        case _ :: Nil => Nil
        case s1 :: s2 :: xs => (s1, s2) :: makePairs(s2 :: xs)
      }


    def initializeMatrix(edgesList: List[(String, String)],
                         gr: Graph[String, LDiEdge]): Graph[String, LDiEdge] =
      edgesList match {
        case y :: ys =>
          initializeMatrix(ys, gr + (y._1 ~+> y._2) (""))
        case Nil =>
          gr
      }

    val edges = details.flatMap(d => makePairs(OperationList(d))).distinct

    initializeMatrix(edges, Graph())
  }
}

object Group {
  val Empty = Group(Nil, Nil)

  def apply(): Group = Group.Empty

  def apply(details: List[Int]): Group =
    new Group(
      details,
      details.flatMap(OperationList(_)).distinct
    )

  def ofDetails(details: List[Int]) = Group(details)


  implicit class GroupCollectionExtender(groups: List[Group]) {
    def getDetails: List[Int] = groups.flatMap(_.details)

    def getOperations: List[String] =
      groups.flatMap(_.operations).distinct

    def show(): Unit = groups foreach println
  }

  implicit class DetailsExtender(details: List[List[Int]]) {
    def makeGroupFromDetails: List[Group] = details.map(Group(_))
  }
}
