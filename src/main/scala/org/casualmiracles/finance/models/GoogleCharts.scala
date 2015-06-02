package org.casualmiracles.finance.models

object GoogleCharts {

  private def chartScale( ys: Stream[Double], upper: Double ): (Double, Double, Stream[Double]) ={
    val ymin = ys.min
    val ymax = ys.max
    val yrange = ymax - ymin
    val yscale = upper/yrange
    
    (ymin, ymax, ys.map( y => (y-ymin)*yscale) ) 
  }
  
  def chartUrl(vs: Stream[Double]): String = {
    val (ymin, ymax, ys) = chartScale( vs, 100 )
    
    "http://chart.apis.google.com/chart?chs=300x200&cht=lc&chxt=x,y&chg=20,25,2,5&chxr=0,0," +
        (vs.length - 1) +
        "|1," + "%3.1f".format(ymin) + "," +
        "%3.1f".format(ymax) + "&chd=t:" +
        ys.map("%3.1f".format(_)).mkString(",")
   }
}
