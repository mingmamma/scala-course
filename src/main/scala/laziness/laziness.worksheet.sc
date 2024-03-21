// Derive the from method of LazyList by the recursive definition with lazy con #::?!
def from(n: Int): LazyList[Int] = n #:: from(n+1)

// accessing natural numbers with the from method
def naturalNumbers: LazyList[Int] = from(0)
val singleDigits: List[Int] = naturalNumbers.take(10).toList

// Implementation of Sieve of Erothosthenes which aims to produce a list of prime numbers with LazyList
// At each recursion step, the head of the given input LazyList is taken to be taken as element of the prime numbers list
// The validity of the determining the head in the given input LazyList as a prime number is following
// In the first turn, 2 is taken as the first prime number
// In subsequent turns, the recursive call dictates that the element of the LazyList as the input of the recusive call is NOT a multiple
// of any existing elements of the previous LazyList, thus entailing the head of that input to be determined as a prime number
def primes: LazyList[Int] = 
    // The accompanying implementation SEEM to be coupled with the LazyList of ints from 2
    // or otherwise the return of the implementation is NOT sensible
    def natsFromTwo = from(2)
    def recursiveSieve(input: LazyList[Int]): LazyList[Int] =
        input.head #:: recursiveSieve(input.tail.filter(ele => ele % input.head != 0))
    recursiveSieve(natsFromTwo)

primes.take(10).toList

// Naive Newton method for square root implementation
def sqrtIter(estimate: Double, x: Double): Double =
    def closeEnough(estimate: Double, target: Double): Boolean =
        (estimate * estimate - target).abs <= 0.001

    def estimateIter(estimate: Double, target: Double): Double =
        (estimate + target/estimate)/2

    // termination condition is determined by the implementation by closeEnough helper
    // which forms a coupling part to make the overall program work
    if closeEnough(estimate, x) then estimate
    else sqrtIter(estimateIter(estimate, x), x)

sqrtIter(1, 2)

// Enhance composability of Newton method for square root with LazyList without consideration of termination condition
def sqrtSeq(x: Double): LazyList[Double] =
    def estimateIter(estimate: Double): Double =
        (estimate + x/estimate)/2
    
    lazy val sqrtEstimates: LazyList[Double] = 1 #:: sqrtEstimates.map(estimate => estimateIter(estimate))
    sqrtEstimates

sqrtSeq(2).take(10).toList