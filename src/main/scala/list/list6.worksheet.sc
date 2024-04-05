// Educational implementation of a linked list in Scala, named LIST to avoid naming conflict with existing standard library implementation

// Covariance
trait LIST[+T]:

    def head: T

    def tail: LIST[T]

    def isEmpty(): Boolean =
        this match
            case LIST.NIL => true
            case _ => false

    // Helps to give the List(...) format
    override def toString(): String = 
        // recurring through the elements of the list to format the representation
        def elementRecur(prefix: String, li: LIST[T]): String =
            li match
                case LIST.NIL => s")"
                case LIST.cons(head, tail) => s"$prefix$head${elementRecur(", ", tail)}"    
        this match
            case LIST.NIL => "LIST()"
            case _ => elementRecur("LIST(", this)

    def toString2(): String =
        def recur(prefix: String, xs: LIST[T]): String =
            xs match
                case LIST.NIL => ")"
                case LIST.cons(y, ys) => s"$prefix$y${recur(", ", ys)}"

        recur("LIST(", this)
            

    // def prepend(elem: T): List[T] = ???


object LIST:
    // Two sub types of List:
    // singleton empty list object
    case object NIL extends LIST[Nothing]:
        def head: Nothing = throw new NoSuchElementException

        def tail: LIST[Nothing] = throw new NoSuchElementException

    // type-generic case class with parameters representing a non-empty list
    case class cons[+T](head: T, tail: LIST[T]) extends LIST[T]

    // Enable creation of an empty list by LIST()
    def apply[T](): LIST[T] =
        NIL

    // Enable creation of a single element list by LIST(someElem)
    def apply[T](elem: T): LIST[T] =
        elem :: NIL

    // Enable creation of a list of two elements
    def apply[T](elemA: T, elemB: T): LIST[T] =
        elemA :: (elemB :: NIL)

    // The three constructor methods are suitable to be put in the companion object
    // since they are class at the class level LIST(...) but not at the object level
    // li.prepend(...). Hence prepend should be defined in the trait
    // def prepend[T](elem: T): LIST[T] = ???

    extension [T](x: T) def ::(xs: LIST[T]): LIST[T] = 
        cons[T](x, xs)

    // function to return the nth element of a LIST, throws an IndexOutOfBoundsException in case
    extension [T](xs: LIST[T]) def nth(n: Int): T =
        if xs.isEmpty() then throw new IndexOutOfBoundsException
        else n match
            case 0 => xs.head
            case _: Int => xs.tail.nth(n-1)
        
        
            

val LISTa = LIST()
val LISTb = LIST(1)
val LISTc = LIST(2, 3)

4 :: LISTc

LISTc.nth(0)
LISTc.nth(1)
// java.lang.IndexOutOfBoundsException
// LISTa.nth(2)

LISTa.toString2()