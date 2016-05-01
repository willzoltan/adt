// File.scala
// This is the data structure of the input files
package IO

import scala.collection.mutable.ArrayBuffer
/** The abstract file **/

trait AbstractFile {
  def row(i: Int) : ArrayBuffer[String]
  def rowCount : Int
  
  def columnNames : ArrayBuffer[String]
  def columnCount : Int
  
  def rowsContaining(s: String): Array[Int]
  
  def equal(i: Int, row: ArrayBuffer[String]): Double   //wheter row i in this file is the same as given row, returning how similar they are
}


/** The implemented File data structure. Input file location
    initialize with File(initLocation) to use default deleimiter
    
    We assume the first line is the names of columns (the header).
**/

class File(initLocation: String, delimiter: String)  extends AbstractFile {
  private val source = io.Source.fromFile(initLocation)
  
  private var rows: ArrayBuffer[ArrayBuffer[String]] = new ArrayBuffer(5)
  private var columns: ArrayBuffer[String] = new ArrayBuffer(1)
  
  private var numberColumns = 0
  private var numberRows = 0
  
  private var size = 5
  
  loadFromSource //load
  
  /** Set default CSV delimiter to "," **/
  def this(i: String) { this(i, ",") }
  
  private def loadFromSource {
    val lines = source.getLines
    
    //load header
    val firstLine: String = lines.next
    columns = parseLine(firstLine)
    
    numberColumns = columns.length
    
    //load data
    for (line <- lines){
      checkSpace
      rows += parseLine(line)
      numberRows += 1
    }
  }
  
  private def parseLine(line: String): ArrayBuffer[String] = {
    val array: Array[String] =  line.split("\"" + delimiter + "\"").map(_.trim)
    val buf: ArrayBuffer[String] = new ArrayBuffer(array.length)
    
    //copy into arraybuffer
    for (v <- array)
      buf += v
    
    //remove first "
    if (buf(0).length > 0 && buf(0)(0)=='"')
      buf(0) = buf(0).drop(1)
      
    //remove last "
    val last = buf.length - 1
    if (buf(last).length > 0 && buf(last).takeRight(1)=="\"")
      buf(last) = buf(last).dropRight(1)
      
    buf
  }
  
  private def checkSpace {
    if (size > numberRows) return

    val n: ArrayBuffer[ArrayBuffer[String]] = new ArrayBuffer(2*size)
    for (i<- 0 until numberRows)
      n += rows(i)
    size = size * 2
    rows = n
  }
  
  def row(i: Int): ArrayBuffer[String] = {
     assert( i>=0 && i < rowCount)
     rows(i)
        }
 
 def returnArray(row_num:Int, col_num:Int): Array[Array[String]] = {
   val big_array = new Array[Array[String]](row_num)
 
   var i = 0
   while (i != row_num) { 
      var entry = new Array[String](col_num)
      var v = row(i)
      for (j <- 0 to col_num -1) {
        entry(j) = v(j).toString
      }
      big_array(i) = entry
      i += 1

   }
   big_array
 }
  
  def rowCount: Int = numberRows
  
  def columnNames: ArrayBuffer[String] = columns
  
  def columnCount = numberColumns
  
  def rowsContaining(s: String): Array[Int] = {
    var matchingRows: Array[Int] = Array()
    for(row <- 0 until numberRows)
      for (col <- 0 until rows(row).length)
        if (rows(row)(col) == s)
          matchingRows = matchingRows :+ row
    matchingRows
  }

  
  def equal(i: Int, row: ArrayBuffer[String]): Double = {
	var j = 0
	var counter = 0
	var perc : Double = 0
	val therow = rows(i)
	while (j < row.length && j < therow.length){
		if (row(j) == therow(j))
			counter += 1
		j += 1
	}
	perc = counter / row.length
	return perc
}

}

