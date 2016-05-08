//Machine.scala
//This is the machine to be included in the main program, that does calculations and matching
import IO.File

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

//Another implementation, files are matched if there rows have distributions of the same shape
class DistributionMachine(source: File, comparator: File) extends PercentageEqualityMachine(source,comparator,100) {

  //stores the distribution of rows of two files
  private val sourceD = new Array[(String,Int)](source.columnCount)
  private val compD = new Array[(String,Int)](comparator.columnCount)
  
  private def calcD(i:Int, j:Int) = {  //translate two rows into values of them same type(all integers), call them the distribution of the row
  
        var k = 0
	var p = 0
	while (p < source.columnCount) {
	  update(source.row(i)(p),sourceD)
	  p += 1
	}
	p = 0 ; k = 0
	while (p < comparator.columnCount) {
	  update(comparator.row(j)(p),compD)
	  p += 1
	}
	
	def update(content:String, dis: Array[(String,Int)]) = {
	  val occ: Option[(String,Int)] = dis.find(_._1 == content)
	  occ match {
	    case Some(v) => { dis(p) = v}
	    case None    => { dis(p) = (content,k) ; k += 1}
      }
	}
  }
	
  override def matchRow(i:Int, j:Int): Boolean = {
    calcD(i,j)
	return (sourceD == compD)
  }

}

//The implemented machine to be used
class EqualityMachine(source: File, comparator: File) extends PercentageEqualityMachine(source, comparator,100)


//The implemented machine to be used
class SingleEqualityMachine(source: File, comparator: File) extends PercentageEqualityMachine(source, comparator,(100/source.columnCount).toInt)
