def last[T](xs: List[T]): T =
    xs match
        case Nil => throw Error("Empty list")
        case List(x) => x
        case head :: next => last(next)

assert(last(List(3, 1, 2)) == 2)

def init[T](xs: List[T]): List[T] =
    xs match
        case Nil => throw Error("Empty list")
        case List(x) => Nil
        case head :: next => head :: init(next)

assert(init(List(3, 1, 2)) == List(3, 1))

extension[T](xs: List[T])
    def ++(ys: List[T]): List[T] = 
        xs match
            case Nil => ys
            case List(x) => x::ys
            case head :: next => head :: next ++ ys

assert(List(1, 3) ++ List(2, 4) == List(1, 3, 2, 4))

extension[T](xs: List[T])
    def reverse: List[T] =
        xs match
            case Nil => Nil
            case head :: next => next.reverse ++ List(head)

assert(List(3, 1, 2).reverse == List(2, 1, 3))

def removeAt[T](n: Int, xs: List[T]): List[T] =
    xs match
        case Nil => Nil
        case head :: next => 
            if n > 0 then head::removeAt(n-1, next)
            else next

assert(removeAt(1, List('a', 'b', 'c', 'd')) == List('a', 'c', 'd'))
assert(removeAt(3, List('a', 'b', 'c', 'd')) == List('a', 'b', 'c'))
assert(removeAt(5, List('a', 'b', 'c', 'd')) == List('a', 'b', 'c', 'd'))

def flatten(xs: List[Any]): List[Any] = 
    xs match
        case Nil => Nil
        case head :: next => 
            head match
                case ys: List[Any] => flatten(ys) ++ flatten(next)
                case _ => List(head) ++ flatten(next)
            
assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))

def flatten2(xs: Any): List[Any] =
    xs match
        case Nil => Nil
        case head :: tail => flatten2(head) ++ flatten2(tail)
        case _ => xs :: Nil

flatten2(List(List(1, 1), 2, List(3, List(5, 8))))