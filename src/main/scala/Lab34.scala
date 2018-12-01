import java.io.{BufferedWriter, File, FileWriter}

import gks._
import gks.{Group, G, Util}
import gks.Group._
import gks.Util._
import Lab12._
import scalax.collection.Graph
import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot._
import scalax.collection.io.dot.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object Lab34 extends App {

  val ZippedGroups = OptimizedGroups.zipWithIndex.map { case (g, i) => (g, (i + 1).toString) }

  val root = DotRootGraph(directed = true, id = None)

  def edgeTransformer(innerEdge: Graph[String, LDiEdge]#EdgeT):
  Option[(DotGraph, DotEdgeStmt)] = innerEdge.edge match {
    case LDiEdge(source, target, label) => label match {
      case label: String =>
        Some((root,
          DotEdgeStmt(source.toString,
            target.toString,
            if (label.nonEmpty)
              List(DotAttr("label", label.toString))
            else
              Nil
          )
        ))
    }
  }

  "graphs".task()
  ZippedGroups foreach {
    case (group, index) =>
      println(s"Graph for group #$index: ${group.getGraph.mkString("; ")}")
      Future {
        val dot = group.getGraph.toDot(root, edgeTransformer)
        import sys.process._

        val file = new File(index.graphDir)
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write(dot)
        bw.close()

        val terminalCommands = List(s"dot -Tpng -O graph/lab3-graph$index.dot.png", s"xdg-open graph/lab3-graph$index.dot.png")

        terminalCommands.foreach(_ !)
      } onComplete {
        case Success(_) => println("Images were opened succesfully")
        case Failure(_) => Unit
      }
  }

  def createGraph(group: Group): Set[G] = {
    val nodes = group.operations.map(G(_))

    def findEdgesForNode(node: G, acc: Set[String] = Set()): G = {
      val all = group.details.map(OperationList(_))

      def findInRow(row: List[String], acc: Set[String] = Set()): Set[String] =
        row match {
          case x :: Nil => acc
          case x1 :: x2 :: rest =>
            if (x1 == node.name) findInRow(x2 :: rest, acc + x2)
            else findInRow(x2 :: rest, acc)
        }

      node ++ all.flatMap(findInRow(_, Set())).toSet
    }

    nodes.map(n => findEdgesForNode(n, Set())).toSet
  }

  val Graphs = OptimizedGroups.map(createGraph)

  def findByFeedbackRelation(graph: List[G], acc: Module = Module.Empty): Module = {
    graph match {
      case Nil =>
        acc
      case edge :: rest =>
        val result =
          graph
            .filter(e =>
              edge.subnodes.contains(e.name))
            .filter(_.subnodes.contains(edge.name))
            .map(_.name)
        if (result.isEmpty)
          findByFeedbackRelation(rest, acc)
        else
          findByFeedbackRelation(rest, acc ++ Module(edge.name :: result))
    }
  }

  val res = findByFeedbackRelation(Graphs.head.toList)
  println(res)


  Thread.sleep(10000000)
}
