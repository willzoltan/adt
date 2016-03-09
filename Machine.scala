//Machine.scala
//This is the machine to be included in the main program, that does calculations and matching
import IO.File

trait Machine {
  //the abstract trait that defines all available methods that can be used 
  //Initiated with a cumulative file, and a list of slices
  def rowInCumulative(i: Int, j:Int)
  //PRE         : whether row j in slice i is contained in cumulative
  //POST		: return the list of indexs of rows that matches (usually only one, hence a unit list), empty list if fails
  
  def sliceInCumulative(i: Int)
  //PRE      	: whether slice i is contained in cumulative,
  //POST 		: return the array of list of indexs of rows that matches the rows of slice (in order), array value = empty if that row isn't matched
  
  def columnInCumulative(i: Int, j: Int)
  //PRE      	: whether column j in slice i is contained in cumulative
  //POST		: return the list of indexs of rows that matches (usually only one, hence a unit list), empty list if fails
  
  
}


//The implemented machine to be used
class SimpleMachine(cumulative: File, slices: List[File]) {
   //Local copy, not necessary
   private val c = cumulative
   private val ss = slices
   
   def rowInCumulative(i:Int, j:Int): List[Int] = {
     val therow = ss(i).row(j)
	 val output: List[Int] = List()
	 for (l <- 0 until (c.rowCount -1)) {
	   if ( c.row(l) == therow ) { output :+ l}
	   //!! equality is not in spec of AbstractFile
	 }
	 return output	 
   }
   
   def sliceInCumulative(i:Int): Array[List[Int]] = {
     val k = ss(i).rowCount
     val output = new Array[List[Int]](k)
	 for (l <- 0 until k-1) {
	   output(l) = rowInCumulative(i,l)
	 }
	 return output
   }
   
   def columnInCumulative(i:Int, j:Int): List[Int] = {
     val thecolumn = ss(i).column(j)
	 //!! file.column is not in spec of AbstractFile
	 val output: List[Int] = List()
	 for (l <- 0 until (c.columnCount -1)) {
	   if (c.column(l) == thecolumn) { output :+ l }
	   //!! equality is not in spec of AbstractFile
	 }
	 return output 
   }


}