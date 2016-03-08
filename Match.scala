// Match.scala
// This is the main programme
//package App

import IO.File
import TABLE.MyTable
import TABLE.MyTableModel
import scala.swing._
import javax.swing.table._

object Match {

  /** Main program for the entire matching application. */
  def main(args: Array[String]) {
    //Starting implementation: Import file from argument and print to console
    
    if (args.length == 1){
      val csv = new File(args(0))
      
      //print header
   //   println(csv.columnNames.mkString("|"))
      
      //print rows
     // for (i <- 0 until csv.rowCount)
     //   {println(csv.row(i).mkString("|"))}

      var v = csv.returnArray(csv.rowCount, csv.columnCount)

    //  var v = csv.rows
    //  v = v.toArray
    //  v = v.map(_.toArray)
      val find = csv.rowsContaining("Def")
      val display = new MyTable(v, csv.columnNames.toList, csv.rowCount, csv.columnCount, find)
      val a = display.top
      a.visible = true

      //find phrase "2016-02-09T22:23:35"
      println("Searching for: '2016-02-09T22:23:35'. Found on lines:")
      
  /*    for (x <- find)
        println(x)
    }else{
      println("Wrong number of arguments")*/
    }
  }
}
