class Rational(numer: Int, denom: Int):

    require(denom > 0, s"denominator must be greater than 0, was $x/$y")
    
    // Simplify the rational by gcd on construction
    // https://en.wikipedia.org/wiki/Euclidean_algorithm
    // Assuming both inputs are postive and expecting a positive return value
    // i.e. -2 % 4 // Int: -2 is possible
    def gcd(a: Int, b: Int): Int =
        if (b == 0) a else gcd(b, a % b) 

    val denominator = denom / gcd(numer.abs, denom) 
    val numerator = numer / gcd(numer.abs, denom)

    def add(that: Rational): Rational = 
        new Rational(numerator*that.denominator + denominator*that.numerator, denominator*that.denominator)

    def subtract(that: Rational): Rational =
        new Rational(numerator*that.denominator - that.numerator*denominator, denominator*that.denominator)

    def multiply(that: Rational): Rational =
        new Rational(numerator*that.numerator, denominator*that.denominator)

    def divide(that: Rational): Rational =
        new Rational(numerator*that.denominator, denominator*that.numerator)

    def neg(): Rational =
        new Rational(-numerator, denominator)

    def less(that: Rational): Boolean =
        numerator*that.denominator < denominator*that.numerator

    def max(that: Rational): Rational =
        if (this.less(that)) that else this

    /*
    Override equals method 
    */    
    
    private def equalRationals(that: Rational): Boolean =
        numerator*that.denominator == denominator*that.numerator

    override def equals(that: Any): Boolean = that match
        case that: Rational => equalRationals(that)
        case _ => false

    /* 
    Override toString method
    */
    override def toString(): String = 
        s"$numerator/$denominator"
end Rational

val x = Rational(1, 3)
val y = Rational(3, 2)
val z = Rational(9, 6)
val w = Rational(2, 3)

assert(x.add(y) == Rational(11, 6))
assert(y.subtract(x) == Rational(7, 6))

assert(y.subtract(z) == Rational(0, 18))
assert(y==z)

assert(x.subtract(w)==Rational(-1, 3))

1 % -1

1 % -2

1 % -3

1 % -4

-2 % 4