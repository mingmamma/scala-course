def mergeSort(xs: List[Int]): List[Int] =
    def merge(xs: List[Int], ys: List[Int]): List[Int] =
        (xs, ys) match
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x::xsTail, y::ysTail) => {
                if x < y then x :: merge(xsTail, ys)
                else y :: merge(xs, ysTail)
            }
        
    val n = xs.length / 2
    if n == 0 then xs
    else
        val (frontXs,backXs) = xs.splitAt(n)
        merge(mergeSort(frontXs), mergeSort(backXs))

// generic mergeSort
def mergeSortGeneric[T](xs: List[T])(lt: (T, T) => Boolean): List[T] =
    def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match
            case (Nil, ys) => ys
            case (xs, Nil) => xs
            case (x::xsTail, y::ysTail) => {
                if lt(x, y) then x :: merge(xsTail, ys)
                else y :: merge(xs, ysTail)
            }
        
    val n = xs.length / 2
    if n == 0 then xs
    else
        val (frontXs,backXs) = xs.splitAt(n)
        merge(mergeSortGeneric(frontXs)(lt), mergeSortGeneric(backXs)(lt))        

    

extension [T](xs: List[T])
    def splitAt(n: Int): Tuple2[List[T], List[T]]=
        (xs.take(n), xs.drop(n))


assert((List(1,3,2,4).splitAt(0)) == (Nil, List(1,3,2,4)))
assert((List(1,3,2,4).splitAt(2)) == (List(1,3), List(2,4)))


assert(mergeSort(List(1,3,2,4)) == List(1,2,3,4))
assert(mergeSort(List(1,7,9,8,3,2,4)) == List(1,2,3,4,7,8,9))

List(11)