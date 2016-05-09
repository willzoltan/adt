//Machine.scala
//This is the machine to be included in the main program, that does calculations and matching
import IO.File
import scala.collection.mutable.ArrayBuffer

trait Machine {
  
  //ABSTRACT: Returns true if source(i) matches with comparator(j)
  def matchRow(i: Int, j: Int): Boolean
  
  //For the ith row in source, this returns a List of the matching indices in comparator
  def rowInComparator(i: Int): List[Int]
  
  //For the source, this returns a mapping from source row to it's matches
  def calculateMatchingRows 
  
  //returns a list of indices for source where that row has at least one match in the comparator
  def calculateRowsWithMatches
}


class RunableMachine(source: File, comparator: File) extends Machine {
  var arrayOfMatches = new Array[List[Int]](source.rowCount);
  var rowsWithMatches: List[Int] = Nil
  
  calculateMatchingRows
  calculateRowsWithMatches
  
  //ABSTRACT: Returns true if source(i) matches with comparator(j)
  def matchRow(i: Int, j: Int): Boolean = false
  
  //For the ith row in source, this returns a List of the matching indices in comparator
  def rowInComparator(i: Int): List[Int] = {
    var matches: List[Int] = Nil
    for (j <- 0 until comparator.rowCount)
      if (matchRow(i, j)){
        matches = j :: matches
      }
    matches.reverse
  }
  
  //For the source, this returns a mapping from source row to it's matches
  def calculateMatchingRows = {
    for (i <- 0 until source.rowCount)
      arrayOfMatches(i) = rowInComparator(i)
  }
  
  //returns a list of indices for source where that row has at least one match in the comparator
  def calculateRowsWithMatches = {
    rowsWithMatches = Nil
    for (i <- 0 until source.rowCount)
      if (arrayOfMatches(i).length > 0)
        rowsWithMatches = rowsWithMatches :+ i
  }
}



//The implemented machine to be used
class PercentageEqualityMachine(source: File, comparator: File, percentage: Int) extends RunableMachine(source, comparator) {
  
  
  
  //Implements matching with equality of all rows
  override def matchRow(i: Int, j: Int): Boolean = {
    var count: Int = 0;
    val threshold: Int = math.ceil((percentage.toFloat/100)*source.columnCount).toInt
    var k: Int = 0;
    var l: Int = 0;
    var matchFound: Boolean = false;
    while ((count < threshold) && (k < source.columnCount)){
      matchFound = false;
      l = 0;
      while ((!matchFound) && (l < comparator.columnCount)){
        if (source.row(i)(k) == comparator.row(j)(l)){
          matchFound = true
          count += 1
        }
        l += 1
      }
      k += 1
    }
    return (count >= threshold)
  }
}

class DistributionMachine(source:File, comparator: File) extends RunableMachine(source, comparator) {
	val sourceD = Array[Int](source.columnCount)
	val compD = Array[Int](comparator.columnCount)
	
	override def matchRow(i: Int, j: Int): Boolean = {
		val row1 =  source.row(i) ; 
		println("Initialization")
		val row2 = comparator.row(j)
		calcD(row1,sourceD) ; calcD(row2,compD)
		return(sourceD == compD)
	}
	
	def calcD(arow: ArrayBuffer[String], dis: Array[Int]) = {
		println("the distribution length is" ++ dis.length.toString())
		var l = 0 ; var k = 0
		while (l < dis.length) {
            var updated = false
			println("Going to update row with element" ++ l.toString())
		    for (p <- 0 to l) {
				if (arow(p) == arow(l)) { dis(l) = dis(p) ; updated = true}
			}
			if (!updated) {dis(l) = k ; k += 1}
			l += 1
		}
	}
	
}

//The implemented machine to be used
class EqualityMachine(source: File, comparator: File) extends PercentageEqualityMachine(source, comparator,100)


//The implemented machine to be used
class SingleEqualityMachine(source: File, comparator: File) extends PercentageEqualityMachine(source, comparator,(100/source.columnCount).toInt)
