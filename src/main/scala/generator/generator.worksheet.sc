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
val integers = new Generator[Int]:
    val rand = java.util.Random()
    def generate(): Int =
        rand.nextInt()

integers.generate()

// derive a boolean rng from integer rng
val booleans = new Generator[Boolean]:
    val intGen = integers
    def generate(): Boolean =
        intGen.generate() % 2 == 0

booleans.generate()

// derive a generator that returns an random element within a range 
def range(low: Int, high: Int) = new Generator[Int]:
    def generate(): Int = 
        val delta = (high - low).abs
        val x = integers.generate().abs
        low + x % delta // exploiting the property that the modulo of delta is one of the integers from 0 to delta -1 inclusive

range(0, 5).generate()

// Exploiting the property of modulo/range in the indexing of 0 to length -1 to derive a random parameter generator from a list of parameters
def oneOf[T](xs: T*) = new Generator[T]:
    def generate() = 
        val randomIndex = range(0, xs.length-1).generate()
        xs(randomIndex)

oneOf("red", "blue", "green").generate()

// rewriting oneOf since it fits the map pattern
def oneOf2[T](xs: T*): Generator[T] =
    range(0, xs.length).map(randomIndex => xs(randomIndex))

oneOf2("red", "blue", "green").generate()

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

// single is a rng that always returns the same single element
def single[T](t: T) = new Generator[T]:
    def generate() = t

// list rng
def lists: Generator[List[Int]] =
    for
        isEmptyList <- booleans
        list <- if isEmptyList then emptyLists else nonemptyLists
    yield 
        list

// generator of empty lists
def emptyLists =
    single(Nil)

// generator of nonempty lists
def nonemptyLists =
    for
        x <- integers
        tail <- lists
    yield
        x :: tail

lists.generate()


// generator of nonEmpty list    

// tree rng
enum Tree:
    case Fork(x: Int, left: Tree, right: Tree)
    case Leaf(x: Int)

def trees: Generator[Tree] =
    for
        isLeaf <- booleans
        tree <- if isLeaf then Leaf else Fork
    yield 
        tree

def Leaf =
    for x <- integers yield Tree.Leaf(x)

def Fork =
    for
        x <- integers
        left <- trees
        right <- trees
    yield
        Tree.Fork(x, left, right)

trees.generate()




