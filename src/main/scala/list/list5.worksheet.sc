// pretty inefficient insertion sort
def isort(xs: List[Int]): List[Int] =
    
    // helper method insert
    def insert(x: Int, xs: List[Int]): List[Int] =
        xs match
            case Nil => List(x)
            case y :: ys => 
                if x <= y then x :: y :: ys
                else y :: insert(x, ys)
        
    
    xs match
        case Nil => Nil
        case List(x) => List(x)
        case y :: ys => insert(y, isort(ys))

isort(List(4,1,3,2))
    
    

