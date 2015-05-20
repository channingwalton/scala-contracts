package org.casualmiracles.finance.models

import org.casualmiracles.finance.contracts._
import PR._
import Stream._

import java.io.File
import java.io.PrintWriter

object Printer {
  def writeFile(file: File)(op: PrintWriter => Unit) {
    val writer = new PrintWriter( file )
    try{
       op(writer)
    }finally{
      writer.close
    }
  }  
}

class LatticeImage extends Zip{

  def getDotFile(pr: PR[Double], baseName: String, imageType: String ) {
    writeTreeAsDot( baseName, pr )
    runDot( baseName, imageType )
  }
  
  private def writeTreeAsDot( baseName: String, pr: PR[Double]) {
    
    // TODO: make our mind for printing it to file (see above, Printer)
    // TODO: move Printer to utilities package?
    //Printer.writeFile(new File("/tmp/" + baseName + dotExt ))(writer=>writer.println( dotGraph( prToDot( pr )) )
    println( baseName + dotExt ) 
    println( dotGraph (prToDot( pr)).map(_ + "\n").mkString("")) 
  }
  
  def runDot( baseName: String, fileType: String ): String =
    // TODO: run system.process
    List( "dot -T", fileType, " -o ", baseName, ".", fileType, " ", baseName, dotExt).mkString("")
  
  // Convert a (PR Double) to a list of dot node relationships.
  def prToDot( pr: PR[Double] ): List[String]  = rvsToDot( pr.unPr )
  
  // Convert lattice to list of dot node relationships.
  def rvsToDot( rvs: Stream[RV[Double]] ): List[String] = {
    val numberedRvs = assignIds( rvs, 1)
    showNodes( numberedRvs ) ++ treeToDot( numberedRvs )
  } 
  
  
  val dotExt = ".dot"
  
  // Number each of the nodes in a lattice.
  def assignIds[A]( s: Stream[RV[A]], n:Int): Stream[RV[(Int, A)]] = s match{ 
    case Empty => Empty 
    case ( rv #:: rvs) => numberList( rv.reverse, n ) #:: assignIds( rvs, n + rv.length ) 
  }
  
  def numberList[A]( l: Stream[A], n : Int): Stream[(Int, A)] = (Stream from n).zip(l)
  
  def showNodes( numberedRvs: Stream[RV[(Int,Double)]]): List[String] = {
    def showSlice( n:Int, sl:RV[(Int, Double)] ):List[String] = "subgraph Slice" + n + "{ rank=same" ::
      sl.map {case (n:Int, d:Double) => n + nodeLabel(d) }.toList :::
      List( "SL" + n + " [label=\"" + n + "\" style = solid peripheries=0] }" )
      
    val nls = numberList(numberedRvs, 0)
    
    nls.flatMap {case (n:Int, sl: RV[(Int, Double)])=>showSlice(n, sl)} toList
  }
  
  // TODO: wrap s into format spec, as per original showFFloat (Just 2) but keep in mind precision setting
  def nodeLabel( s: Double ): String = " [label=\"" + s + "\"]"
  
  def treeToDot[A]( s: Stream[RV[(Int,A)]] ): List[String] = s match{
    case ( a #:: Empty ) => List()
    case (a #:: b #:: rest) => dotJoin( a, b.take(a.length) ) ++
                               dotJoin( a, (b.tail)) ++
                               treeToDot( b #:: rest)
  }
  
  def dotJoin[A]( a: RV[(Int, A)], b: RV[(Int, A)]): List[String] = {
    zipWith(a, b)( _._1 + " -- " + _._1 ).toList
  }
  
  def dotGraph( body: List[String]): List[String] = 
    dotGraphHdr ++ body.map( s => formatDotStmt(s) ) ++ List( "}" )
  
  def dotGraphHdr(): List[String] = List( "graph contract_lattice {" ,
                 "  rankdir=LR;" ,
                 "  dir=none;" ,
                 "  node [style=filled color=pink shape=box fontsize=10 width=0.5 height=0.4];")
                 
  def formatDotStmt( s: String): String = "  " + s + ";"
}
