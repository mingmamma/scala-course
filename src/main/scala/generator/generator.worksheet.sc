trait Generator[+T]:
    def generate(): T

extension[T, S](g: Generator[T])

    def map(f: T => S) = new Generator[S]:
        def generate(): S =
            f(g.generate())

    def flatMap(f: T => Generator[S]) = new Generator[S]:
        def generate(): S =
            f(g.generate()).generate()

// make use of Java random integer generator            
val integersGen = new Generator[Int]:
    val rand = java.util.Random()
    def generate(): Int =
        rand.nextInt()

integersGen.generate()

// derive a boolean rng from integer rng
val booleansGen = new Generator[Boolean]:
    val intGen = integersGen
    def generate(): Boolean =
        intGen.generate() % 2 == 0

booleansGen.generate()

// derive a generator that returns an random element within a range of integers
// where possible range is low (inclusive) to high (exclusive)
// making use of the property of integer divison such that the quotient is an integer strictly smaller
// than the divisor s.t it can be an integer in the range of 0 to delta - 1
def range(low: Int, high: Int) = new Generator[Int]:
    def generate(): Int = 
        val delta = (high - low).abs
        val x = integersGen.generate().abs
        low + x % delta

range(0, 5).generate()

def oneOf[T](xs: T*) = new Generator[T]:
    def generate() = 
        // the following would be incorrect following the precise defintion of range Generator
        // val randomIndex = range(0, xs.length-1).generate()
        val randomIndex = range(0, xs.length).generate()
        xs(randomIndex)

oneOf("red", "blue", "green").generate()

// Equivalent implementation of oneOf Generator with map in place of generate() method definition boilerplate
def oneOf2[T](xs: T*): Generator[T] =
    range(0, xs.length).map(randomIndex => xs(randomIndex))

oneOf2("red", "blue", "green").generate()    

// Equivalent implementation of oneOf Generator with for comprehension in place of low-level map method
def oneOf3[T](xs: T*): Generator[T] =
    for 
        randomIndex <- range(0, xs.length)
    yield
        xs(randomIndex)

oneOf3("red", "blue", "green").generate()

// derive a generator for a tuple pair of (T, U), from two given generators t: Generator[T] and u: Generator[U]
// Derivation:
// for x <- t; y <- u yield (x, y) GIVES:
// t.flatMap(x => u.map(y => (x, y))), within the outer parentheses GIVES:
// x => u.map(y => (x, y)) GIVES by the definition of map:
// new Generator[(T, U)]: {def generate(): (T, U) = (x, u.generate())} GIVES the subsequent results from the original expression 
// when substituted back, as ready to be applied as the parameter of flatMap 
// new Generator[(T, U)]: {def generate(): (T, U) = (new Generator[(T, U)]: {def generate(): (T, U) = (t.generate(), u.generate())}).generate()} SIMPLIFIES to:
// new Generator[(T, U)]: {def generate(): (T, U) = (t.generate(), u.generate())} QED
def pair[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = 
    for
        x <- t
        y <- u
    yield
        (x, y)

// single is a generator that always returns the same single element
def single[T](t: T) = new Generator[T]:
    def generate() = t

// a generator of linked list is the first example of random value generator 
// of a recursive data structure
def listsGen: Generator[List[Int]] =
    for
        isEmptyList <- booleansGen
        list <- if isEmptyList then emptyListsGen else nonemptyListsGen
    yield 
        list

// generator of empty lists
def emptyListsGen =
    single(Nil)

// generator of nonempty lists
def nonemptyListsGen =
    for
        x <- integersGen
        tail <- listsGen
    yield
        x :: tail

listsGen.generate()

// picks a random generator from a Seq of random generators
def oneOfGens[T](gens: Seq[Generator[T]]): Generator[T] =
    range(0, gens.length).flatMap(randomIndex => gens(randomIndex))

// Equivalent implementation of OneOfGens by using map in place of flatMap with
// consideration to type compatability
def oneOfGens2[T](gens: Seq[Generator[T]]): Generator[T] =
    range(0, gens.length).map(randomIndex => gens(randomIndex).generate())    

// Equivalent implementation of listGen by making use of `OneOf` utility
// and define emptyListsGen and nonemptyListsGen IN-PLACE
// following ArbitraryHeaps.scala in quickcheck
def listGen2: Generator[List[Int]] =
    // Could have used the equivalent implementation of OneOfGens 
    // oneOfGens2(Vector(
    oneOfGens(Vector(
        //
        single(Nil),
        //
        for
            nonEmptyHeadInt <- integersGen
            tail <- listGen2
        yield
            nonEmptyHeadInt :: tail)            
    )

listGen2.generate()
   
// a random generator of binary trees is a good second example of random value generator
// of recursive data types. The conception of it can follow natually of that of linked list
// random value generator, by considering the dual cases of the recursion at each node in
// consideration: either a Leaf node or a non-leaf node followed by another tree (hence a Fork node), 
// in the comparable manner as in the linked list, where each node in consideration is either a Nil
// a nonEmpty node followed by a linked list (hence a Con node). After all, it can be remarked that the
// similarity of the structure arise from the fact the the linked list IS a degenerated instance of a tree
enum Tree:
    case Fork(x: Int, left: Tree, right: Tree)
    case Leaf(x: Int)

def trees: Generator[Tree] =
    for
        isLeaf <- booleansGen
        tree <- if isLeaf then Leaf else Fork
    yield 
        tree

def Leaf =
    for x <- integersGen yield Tree.Leaf(x)

def Fork =
    for
        x <- integersGen
        left <- trees
        right <- trees
    yield
        Tree.Fork(x, left, right)

trees.generate()




