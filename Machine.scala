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
  var rowsWithMatches = Nil
  
  calculateMatchingRows
  calculateRowsWithMatches
  
  //ABSTRACT: Returns true if source(i) matches with comparator(j)
  def matchRow(i: Int, j: Int): Boolean = false
  
  //For the ith row in source, this returns a List of the matching indices in comparator
  def rowInComparator(i: Int): List[Int] = {
    var matches: List[Int] = Nil
    for (j <- 0 until comparator.rowCount)
      if (matchRow(i, j))
        matches :+ j
    matches
  }
  
  //For the source, this returns a mapping from source row to it's matches
  def calculateMatchingRows = {
    for (i <- 0 until source.rowCount)
      arrayOfMatches(i) = rowInComparator(i)
  }
  
  //returns a list of indices for source where that row has at least one match in the comparator
  def calculateRowsWithMatches = {
    var rowsWithMatches = Nil
    for (i <- 0 until source.rowCount)
      if (arrayOfMatches(i).length > 0)
        rowsWithMatches :+ i
  }
}



//The implemented machine to be used
class PercentageEqualityMachine(source: File, comparator: File, percentage: Int) extends RunableMachine(source, comparator) {
  
  val threshold: Int = math.ceil((percentage/100)*source.columnCount).toInt
  
  //Implements matching with equality of all rows
  override def matchRow(i: Int, j: Int): Boolean = {
    var count: Int = 0;
    
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


//The implemented machine to be used
class EqualityMachine(source: File, comparator: File) extends PercentageEqualityMachine(source, comparator,100)


//The implemented machine to be used
class SingleEqualityMachine(source: File, comparator: File) extends PercentageEqualityMachine(source, comparator,(100/source.columnCount).toInt)