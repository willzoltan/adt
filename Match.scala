// Match.scala
// This is the main programme
package App

import IO.File

object Match {

  /** Main program for the entire matching application. */
  def main(args: Array[String]) {
    //Starting implementation: Import file from argument and print to console
    
    if (args.length == 1){
      val csv = new File(args(0))
      
      //print header
      println(csv.columnNames.mkString("|"))
      
      //print rows
      for (i <- 0 until csv.rowCount)
        println(csv.row(i).mkString("|"))
      
      //find phrase "2016-02-09T22:23:35"
      println("Searching for: '2016-02-09T22:23:35'. Found on lines:")
      val find = csv.rowsContaining("2016-02-09T22:23:35")
      for (x <- find)
        println(x)
    }else{
      println("Wrong number of arguments")
    }
  }
}
