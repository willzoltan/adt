//Machine.scala
//This is the machine to be included in the main program, that does calculations and matching
import IO.File

trait Machine {
  //the abstract trait that defines all available methods that can be used 
  //Initiated with a cumulative file, and a list of slices
  //Indexing all starts at 0
  def rowInCumulative(i: Int, j:Int): (Int,Double)
  //PRE         : whether row j in slice i is contained in cumulative
  //POST		: return the index of row in culumative that matches, -1 if none, and a percentage of accuracy, 0 if it's not a match
  
  def sliceInCumulative(i: Int): Array[(Int, Double)]
  //PRE      	: whether slice i is contained in cumulative,
  //POST 		: return the array of tuples that matches rows in slice in to rows in cumulative (in order) , (-1,0) if it doesn't match any row
  
  
}


//The implemented machine to be used
class SimpleMachine(c: File, ss: List[File]) {
   //Local copy, not necessary
   //private val c = cumulative
   //private val ss = slices
   
   def rowInCumulative(i:Int, j:Int): (Int,Double) = {
     val therow = ss(i).row(j)
	 var output: (Int,Double) = (-1,0)
	 for (l <- 0 until (c.rowCount -1)) {
	   if ( c.equal(l,therow) == 1 ) { output = (l,1) }
	 }
	 return output	 
   }
   
   def sliceInCumulative(i:Int): Array[(Int,Double)] = {
     val k = ss(i).rowCount
     val output = new Array[(Int,Double)](k)
	 for (l <- 0 until k-1) {
	   output(l) = rowInCumulative(i,l)
	 }
	 return output
   }
   
   /*def columnInCumulative(i:Int, j:Int): List[Int] = {
     val thecolumn = ss(i).column(j)
	 //!! file.column is not in spec of AbstractFile
	 val output: List[Int] = List()
	 for (l <- 0 until (c.columnCount -1)) {
	   if (c.column(l) == thecolumn) { output :+ l }
	   //!! equality is not in spec of AbstractFile
	 }
	 return output 
   }*/


}