// reduceLeft for reduction ops
def sum(xs: List[Int]) = xs.reduceLeft(_+_)
def product(xs: List[Int]) = xs.reduceLeft(_*_)
sum(List(3,1,2,4))
product(List(3,1,2,4))

// Use foldLeft
def sum2(xs: List[Int]) = xs.foldLeft(0)(_+_)
def product2(xs: List[Int]) = xs.foldLeft(1)(_*_)
sum(List(3,1,2,4))
product(List(3,1,2,4))

// improving reverse from quadratic to linear runtime with foldLeft
extension[T](xs: List[T])
    def reverse: List[T] =
        xs match
            case Nil => Nil
            case head :: next => next.reverse ++ List(head)

    def reverse2: List[T] =
        xs.foldLeft(List[T]())((ys, y) => y::ys)

List(3, 1, 2).reverse

// Implement Map and Length functionality with foldRight
def mapFunc[T, U](xs: List[T], f: T => U): List[U] =
    xs.foldRight(List[U]())((y, ys) => f(y) :: ys)

mapFunc(List(3, 1, 2), x => x*x)

def lengthFunc[T](xs: List[T]): Int =
    xs.foldRight(0)((y, n) => n + 1)

lengthFunc(List(3,1,2))