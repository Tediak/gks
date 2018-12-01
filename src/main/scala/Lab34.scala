import java.io.{BufferedWriter, File, FileWriter}

import Lab12._
import com.sun.org.apache.xerces.internal.impl.xs.models.XSAllCM
import gks._
import gks.Util._
import scalax.collection.Graph
import scalax.collection.edge.LDiEdge
import scalax.collection.io.dot._
import scalax.collection.io.dot.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object Lab34 extends App {

  val ZippedGroups = OptimizedGroups.zipWithIndex.map { case (g, i) => (g, (i + 1).toString) }

  //  OptimizedGroups.head.getGraph.edges.toOuter.map(_.to).foreach(println)

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
  "graphs".task()

  val AllGraphs = OptimizedGroups.map(_.getGraph)


  val findReverseModule = AllGraphs.map { graph =>
    graph.edges.filter { edge =>
      val reverseEdge = LDiEdge(edge._2, edge._1)("")
      graph.edges.contains(reverseEdge)
    }.toList.flatMap(edge => List(edge._1, edge._2))
  }.map(r => Module(r.distinct.map(_.toString)))

  println("Reverse modules:")
  findReverseModule.zipWithIndex foreach { p =>
    println(s"#${p._2}: ${p._1}")
  }


  val findAllCycles = AllGraphs.map { graph =>
    graph.nodes.map { node =>
      node.findCycle
    }.filter(_.nonEmpty).flatten
  }

  val findCycleModules = findAllCycles.map { set =>
    val acc: Set[String] = Set()
    set.foldLeft(acc) { (set, cycle) =>
      set ++ cycle.nodes.map(_.value).toSet
    }
  }.map(set => Module(set.toList))

  val filterCycleModules =
    (findReverseModule zip findCycleModules).map {
      case (rev, cycle) =>
        Module(cycle.operations.filterNot(rev.operations.contains))
    }

  println("Cycle modules: ")
  filterCycleModules.zipWithIndex foreach { p =>
    println(s"#${p._2}: ${p._1}")
  }

  val findOnlyIn = AllGraphs.map { graph =>
    val nodesIn = graph.edges.map(_._2.value).toList
    val nodesOut = graph.edges.map(_._1.value).toList

    Module(nodesIn.filterNot(nodesOut.contains).distinct)
  }


  val findOnlyOut = AllGraphs.map { graph =>
    val nodesIn = graph.edges.map(_._2.value).toList
    val nodesOut = graph.edges.map(_._1.value).toList

    Module(nodesOut.filterNot(nodesIn.contains).distinct)
  }

  val filterOnlyIn =
    (findOnlyIn zip (filterCycleModules zip findReverseModule))
      .map {
        case (in, (cycle, reverse)) =>
          Module(in.operations
            .filter(op =>
              !cycle.operations.contains(op) &&
                !reverse.operations.contains(op)
            )
          )
      }

  val filterOnlyOut =
    (findOnlyOut zip (filterCycleModules zip findReverseModule))
      .map {
        case (out, (cycle, reverse)) =>
          Module(out.operations
            .filter(op =>
              !cycle.operations.contains(op) &&
                !reverse.operations.contains(op)))
      }


  println("Only in: ")
  filterOnlyIn.zipWithIndex foreach {
    case (m, i) => println(s"#$i : $m")
  }

  println("Only out: ")
  filterOnlyOut.zipWithIndex foreach {
    case (m, i) => println(s"#$i : $m")
  }

  val findAllChains = AllGraphs.map { graph =>
    Module(graph.nodes.map(_.value).toList)
  }

  val filterChains =
    findAllChains
      .zip(findReverseModule)
      .zip(filterCycleModules)
      .zip(filterOnlyIn)
      .zip(filterOnlyOut)
      .map {
        case ((((chains, m2), m3), m4), m5) =>
          val deprecatedOperations = (m2 ++ m3 ++ m4 ++ m5).operations

          Module(chains.operations.filterNot(deprecatedOperations.contains))
      }

  filterChains.zipWithIndex foreach {
    case (m, i) => println(s"#$i : $m")
  }

  "all modules".task()

  val AllModules =
    findReverseModule
      .zip(filterCycleModules)
      .zip(filterOnlyIn)
      .zip(filterOnlyOut)
      .zip(filterChains) map {
      case ((((m1, m2), m3), m4), m5) =>
        List(m1, m2, m3, m4, m5).filter(_.nonEmpty)
    }

  AllModules.zipWithIndex.foreach{
    case (lst, i) =>
      println(s"#$i")
      lst.foreach(println)
      println("--------------")
  }
  //  Thread.sleep(10000000)
}
