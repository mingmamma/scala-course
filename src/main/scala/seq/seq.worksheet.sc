val M = 3
val N = 2

// Noting that the default type for the result of Seq of pairs
// is chosen to be Vector by runtime
(1 to M).flatMap( x =>
    (1 to N).map(
        y => (x,y)
    )
)

// Implement inner product of two vectors
def innerProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    xs.zip(ys).map(_ * _).sum

// use for syntax
def innerProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
    (for (x, y) <- xs.zip(ys) yield x * y).sum


assert(innerProduct(Vector(1.0,1.0), Vector(2.0,-1.0)) == 1.0)

// Concise but inficient way to determine isPrime by
// checking only n and 1 are divisors of n
def isPrime(n: Int): Boolean = (2 until n).forall(x => n % x != 0) // (2 to n-1)

isPrime(3)
isPrime(7)
isPrime(8)

// Getting pairs of i, j s.t. 0 < j < i < n s.t. i+j is a prime
def genPairofPrimeSum(n: Int): Seq[(Int, Int)] =
    (1 until n).flatMap( i =>
        (1 until i).map( j =>
            (i, j))).filter((x, y) =>
                isPrime(x+y))

def genPairofPrimeSum2(n: Int): Seq[(Int, Int)] =
    for 
        i <- 1 until n // generator
        j <- 1 until i // generator
        if isPrime(i+j) // filter
    yield (i, j)

genPairofPrimeSum(5)
genPairofPrimeSum2(5)



// return all n queen solutions
def queens(n: Int) =
    // Get the placed queens as a set of lists
    // done in the previous k steps
    def getPlacedQueens(k : Int): Set[List[Int]] = 
        if k == 0 then Set(List())
        else
            for
                previousQueens <- getPlacedQueens(k-1)
                col <- 0 until n
                if isSafe(col, previousQueens)
            yield col :: previousQueens
    getPlacedQueens(n)


def isSafe(col: Int, previousQueens: List[Int]): Boolean =
    // Recursively check if the col (on the current previousQueen.length row)
    // is safe/not in check with the previous queens given in the list
    def isInCheck(col: Int, previousQueens: List[Int], delta: Int): Boolean =
        previousQueens match
            case Nil => false
            // is in Check if one of three cases holds:
            // col repeats a queen, col is in (either) diagonal
            // with a queen, col is in check with any earlier queens
            case head :: next => 
                head == col ||
                math.abs(col-head) == delta ||
                isInCheck(col, next, delta + 1)      
    !isInCheck(col, previousQueens, 1)

queens(1)
queens(2)
queens(3)
queens(4)
queens(5)
queens(6)




        
    



            