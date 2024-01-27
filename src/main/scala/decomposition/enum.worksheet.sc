// object Expr:        
//     case class Num(n: Int) extends Expr
//     case class Sum(e1: Expr, e2: Expr) extends Expr
//     case class Product(e1: Expr, e2: Expr) extends Expr
//     case class Var(v: String) extends Expr

// Equivalent Algebraic Data Type(a.k.a pure-data/no-method hierachy of case class) definition with enum
enum Expr:
    case Num(n: Int)
    case Sum(e1: Expr, e2: Expr)
    case Product(e1: Expr, e2: Expr)
    case Var(v: String)

def eval(expr: Expr): Int =
    expr match
        case Expr.Num(n) => n
        case Expr.Sum(e1, e2) => eval(e1) + eval(e2)
        case Expr.Product(e1, e2) => eval(e1) * eval(e2)
        case _: Expr.Var => throw Error("Evaluation of undefined var")

def show(expr: Expr): String =
    expr match
        case Expr.Num(n) => s"$n"
        case s: Expr.Sum => show(s.e1) + " + " + show(s.e2)
        case p: Expr.Product =>
            (p.e1, p.e2) match
                case (s1: Expr.Sum, s2: Expr.Sum) => "(" + show(s1) + ")" + " * " + "(" + show(p.e2) + ")"
                case (s1: Expr.Sum, _) => "(" + show(s1) + ")" + " * " + show(p.e2)
                case (_, s2: Expr.Sum) => show(p.e1) + " * " + "(" + show(s2) + ")"
                case (_, _) => show(p.e1) + " * " + show(p.e2)
        case Expr.Var(v) => v

val sum_2_x_times_y = Expr.Product(Expr.Sum(Expr.Num(2), Expr.Var("x")), Expr.Var("y"))
show(sum_2_x_times_y)

// However, enums can also take parameters and methods
enum Direction(val x: Int, val y: Int):
    case Left extends Direction(-1, 0)
    case Down extends Direction(0, -1)    
    case Right extends Direction(1, 0)
    case Up extends Direction(0, 1)

    // left to down, down to right, right to up, up to left
    // by taking the case of original index (.ordinal) plus one
    // wrappred around the array of distinct cases (.values)
    def turnLeft = Direction.values((ordinal+1) % Direction.values.length)

    def turnRight = Direction.values((ordinal+3) % Direction.values.length)

    def turnAround = Direction.values((ordinal+2) % Direction.values.length)

val down = Direction.Down
down.turnLeft
down.turnRight
down.turnAround

// Equivalent illustrative definition given by the compiler
// abstract class Direction2(val x: Int, val y: Int):
//     def turnLeft = Direction2.values((ordinal+1) % Direction2.values.length)

//     // compiler generated the following as well
//     def values
//     def ordinal

// object Direction2:
//     val Left = new Direction2(-1, 0) {}
//     val Down = new Direction2(0, -1) {}
//     val Right = new Direction2(1, 0) {}
//     val Up = new Direction2(0, 1) {}            