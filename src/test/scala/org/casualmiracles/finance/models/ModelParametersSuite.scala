package org.casualmiracles.finance.models

import org.scalatest._
import Matchers._

/**
 * @author yl
 */
class ModelParametersSuite extends FunSuite with Matchers {
  
  
  test("Basic Instance Functionality"){
    
    val mps = new ModelParameters(0.05, 0.25, 0, 1.0/5.0)
    
    mps.toString should be (
        "sigma: 0.25; r: 0.05; div: 0.0; step: 0.2; up: 1.118292981373268; down: 0.8942200448866237; p: 0.5169304424430043")
  }
  
  test("Lattice parameters override"){
    val mps = new ModelParameters( r=0.5, sigma=0.5, step=1.0/5.0).up(1.1122).down(0.8883).p(0.52)
    
    mps.toString() should be (
      "sigma: 0.5; r: 0.5; div: 0.0; step: 0.2; up: 1.1122; down: 0.8883; p: 0.52"    
    )
  }  
  
  test("Lattice parameters defaults with override"){
    val mps = new ModelParameters().up(1.1122).down(0.8883).p(0.52)
    
    mps.toString() should be (
      "sigma: 0.0; r: 0.0; div: 0.0; step: 0.0; up: 1.1122; down: 0.8883; p: 0.52"    
    )
  }
}