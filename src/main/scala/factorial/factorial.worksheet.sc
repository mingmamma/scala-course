import scala.annotation.tailrec

def factorial(n: Int): Int =
    if n == 0 then 1 else n * factorial(n-1)

factorial(3)


def factorial_tailrec(n: Int): Int =
    @tailrec
    def factorial_accum(n: Int, accum: Int): Int =
        if n == 0 then accum else factorial_accum(n-1, n * accum)
    factorial_accum(n, 1)

factorial_tailrec(3)
