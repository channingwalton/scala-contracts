package org.casualmiracles.finance.models

import org.scalatest._
import Matchers._

import org.casualmiracles.finance.contracts._
import PR._

class LatticeImageSuite extends FunSuite with Matchers {
  
  test("Pr1 Lattice Image dot file") {
         
    val pr1 = PR( Stream[RV[Double]]( 
        Stream[Double](8.641), 
        Stream[Double](9.246,8.901),
        Stream[Double](9.709,9.524,9.346), 
        Stream[Double](10,10,10,10)
      ) )
      
    val li = new LatticeImage()
    val dotImageString = li.dotGraph ( li.prToDot( pr1 )).map(_ + "\n").mkString("")
    
    val dotTestString =  """graph contract_lattice {
                           |  rankdir=LR;
                           |  dir=none;
                           |  node [style=filled color=pink shape=box fontsize=10 width=0.5 height=0.4];
                           |  subgraph Slice0{ rank=same;
                           |  1 [label="8.641"];
                           |  SL0 [label="0" style = solid peripheries=0] };
                           |  subgraph Slice1{ rank=same;
                           |  2 [label="8.901"];
                           |  3 [label="9.246"];
                           |  SL1 [label="1" style = solid peripheries=0] };
                           |  subgraph Slice2{ rank=same;
                           |  4 [label="9.346"];
                           |  5 [label="9.524"];
                           |  6 [label="9.709"];
                           |  SL2 [label="2" style = solid peripheries=0] };
                           |  subgraph Slice3{ rank=same;
                           |  7 [label="10.000"];
                           |  8 [label="10.000"];
                           |  9 [label="10.000"];
                           |  10 [label="10.000"];
                           |  SL3 [label="3" style = solid peripheries=0] };
                           |  1 -- 2;
                           |  1 -- 3;
                           |  2 -- 4;
                           |  3 -- 5;
                           |  2 -- 5;
                           |  3 -- 6;
                           |  4 -- 7;
                           |  5 -- 8;
                           |  6 -- 9;
                           |  4 -- 8;
                           |  5 -- 9;
                           |  6 -- 10;
                           |}
                           |""".stripMargin
  
    dotImageString should equal( dotTestString ) 
  }
}