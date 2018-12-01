import java.io.{BufferedWriter, File, FileWriter}

import gks._
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

  def findByFeedbackRelation(graph: List[String], acc: Module): Module = {
      case Nil =>
        acc
      case edge :: rest =>
        acc

  }

  def getModules(group: Group, acc: Module = Module.Empty): Module = {
    println(s"GRAPH  ${group.getGraph.edges.toList.mkString}")



    acc
  }

  getModules(OptimizedGroups.head)


  Thread.sleep(10000000)
}
