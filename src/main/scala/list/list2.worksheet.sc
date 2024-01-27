// Two functions for illustrating Map
def squareList(xs: List[Int]): List[Int] =
    xs match
        case Nil => Nil
        case y :: ys => y * y ::squareList(ys)

def sqaureList2(xs: List[Int]): List[Int] =
    xs.map(x => x * x)

// simplified generalized higher-order functions for illustrations
extension[T](xs: List[T])
    def map[U](f: T => U): List[U] =
        xs match
            case Nil => Nil
            case y :: ys => f(y) :: ys.map(f)

extension[T](xs: List[T])
    def filter(predicate: T => Boolean): List[T] =
        xs match
            case Nil => Nil
            case y :: ys => if predicate(y) then y :: ys.filter(predicate) else ys.filter(predicate)

// Demo partition, takeWhile, dropWhile, span methods
val numList = List(1,2,3,4)

// partition the list into two lists of evens and odds
numList.partition(x => x % 2 == 0)

// take odd elements until not
numList.takeWhile(x => x % 2 != 0)

// drop odd elements until not and keep the rest
numList.dropWhile(x => x % 2 != 0)

numList.span(x => x % 2 == 0)

// implement the "packing" functionality with takeWhile and dropWhile
def pack[T](xs: List[T]): List[List[T]] =
    xs match
        case Nil => Nil
        case y :: ys => xs.takeWhile(x => x == y) :: pack(ys.dropWhile(x => x==y))
    
// span can do takeWhile and dropWhile in one go :)
def pack2[T](xs: List[T]): List[List[T]] =
    xs match
        case Nil => Nil
        case y :: ys => 
            // extract more elements identical to y and the rest with span
            val (moreY, remaining) = xs.span(x => x==y)
            moreY :: pack(remaining)

pack(List("a","a","a","b","c","c","a"))
pack2(List("a","a","a","b","c","c","a"))            


// Further implement the run-length encoding with pack
def encode[T](xs: List[T]): List[Tuple2[T, Int]] =
    val packed = pack2(xs)
    packed.map(sublist => (sublist.head, sublist.length))

    // def encodeHelper[T](packed: List[List[T]]): List[Tuple2[T, Int]] = 
    // packed match
    //     case Nil => Nil
    //     case subList :: moreSubLists =>
    //         (subList.head,subList.length) :: encodeHelper(moreSubLists)   
    // encodeHelper(packed)

encode(List("a","a","a","b","c","c","a"))    


        
    
    

    
         
    
    