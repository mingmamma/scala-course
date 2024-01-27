// Arithmetic expression interpreter
trait Expr:
    def eval(): Int =
        this match
            case Expr.Num(n) => n
            case Expr.Sum(e1, e2) => e1.eval() + e2.eval()
            case Expr.Product(e1, e2) => e1.eval() * e2.eval()
            case _: Expr.Var => throw Error("Evaluation of undefined var")

    def show(): String =
        this match
            case Expr.Num(n) => s"$n"
            case s: Expr.Sum => s.e1.show() + " + " + s.e2.show()
            case p: Expr.Product =>
                (p.e1, p.e2) match
                    case (s1: Expr.Sum, s2: Expr.Sum) => "(" + s1.show() + ")" + " * " + "(" + p.e2.show() + ")"
                    case (s1: Expr.Sum, _) => "(" + s1.show() + ")" + " * " + p.e2.show()
                    case (_, s2: Expr.Sum) => p.e1.show() + " * " + "(" + s2.show() + ")"
                    case (_, _) => p.e1.show() + " * " + p.e2.show()
            case Expr.Var(v) => v
            
                
object Expr:        
    case class Num(n: Int) extends Expr
    case class Sum(e1: Expr, e2: Expr) extends Expr
    case class Product(e1: Expr, e2: Expr) extends Expr
    case class Var(v: String) extends Expr


val num2 = Expr.Num(2)
num2.eval()
num2.show()

val sum_1_2 = Expr.Sum(Expr.Num(1), Expr.Num(2))
sum_1_2.eval()
sum_1_2.show()

val prod_2_1 = Expr.Product(Expr.Num(2), Expr.Num(1))
prod_2_1.eval()
prod_2_1.show()

val sum_1_2_times_3 = Expr.Product(Expr.Sum(Expr.Num(1), Expr.Num(2)), Expr.Num(3))
sum_1_2_times_3.eval()
sum_1_2_times_3.show()

val add_y_to_2_times_x = Expr.Sum(Expr.Product(Expr.Num(2), Expr.Var("x")), Expr.Var("y"))
add_y_to_2_times_x.show()

val sum_2_x_times_y = Expr.Product(Expr.Sum(Expr.Num(2), Expr.Var("x")), Expr.Var("y"))
sum_2_x_times_y.show()

    

