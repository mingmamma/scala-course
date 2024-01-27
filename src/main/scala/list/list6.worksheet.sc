// Illustrative list from first principles

// Covariance
trait ListM[+T]:

    def isEmpty(): Boolean =
        this match
            case ListM.NilM => true
            case _ => false

    // Helps to give the List(...) format
    override def toString(): String = 
        // recurring through the elements of the list to format the representation
        def elementRecur(prefix: String, li: ListM[T]): String =
            li match
                case ListM.NilM => s")"
                case ListM.cons(head, tail) => s"$prefix$head${elementRecur(", ", tail)}"    
        this match
            case ListM.NilM => "ListM()"
            case _ => elementRecur("ListM(", this)

    // def prepend(elem: T): ListM[T] = ???


object ListM:
    // Two sub types of List:
    // singleton empty list object
    case object NilM extends ListM[Nothing]
    // type-generic case class with parameters representing a non-empty list
    case class cons[+T](head: T, tail: ListM[T]) extends ListM[T]

    // Enable creation of an empty list by ListM()
    def apply[T](): ListM[T] =
        NilM

    // Enable creation of a single element list by ListM(someElem)
    def apply[T](elem: T): ListM[T] =
        elem :: NilM

    // Enable creation of a list of two elements
    def apply[T](elemA: T, elemB: T): ListM[T] =
        elemA :: (elemB :: NilM)

    // The three constructor methods are suitable to be put in the companion object
    // since they are class at the class level ListM(...) but not at the object level
    // li.prepend(...). Hence prepend should be defined in the trait
    // def prepend[T](elem: T): ListM[T] = ???

    extension [T](x: T) def ::(xs: ListM[T]): ListM[T] = cons[T](x, xs)
            

val emp = ListM()
val one = ListM(1)
val twoThree = ListM(2, 3)
4 :: twoThree