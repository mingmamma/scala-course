// parameters in the class signature are parameters used in Rationals' primary constructor
class Rational(x: Int, y: Int):

    // the require method is meant to check required precondition as contract of the implementation with the caller
    // in this case, it is understood that the code caller of Rational class MUST construct Rationals with the supposed
    // parameter of denomenator to be > 0
    require(y > 0, s"denominator must be greater than 0, was $x/$y")
    
    // Simplify the rational by gcd on construction
    // https://en.wikipedia.org/wiki/Euclidean_algorithm
    // This implementation only deal with cases where both inputs are nonnegative Ints 
    // s.t. they are not both zero but one of them can be and the implementation returns a positive Int

    // the method has private visibility with the consideration s.t. its only use case is to derive the fields of simplified
    // numer and denum in class instance constructiion statements. That means it should be only accessed from the class body code
    private def gcdOfNonnegInts(a: Int, b: Int): Int =
        if (b == 0) a else gcdOfNonnegInts(b, a % b) 

    // Fields to model the simplified values of numer and denom of Rationals that forms the canonical representation of Rationals
    // to address the issue that using naive constructor parameters can lead to non-simplified Rationals
    // With the required precondition of constructor parameter, it is entailed that the two numer field could be any Int and the
    // denom field is guaranteed to be postive Int, which is sensible for the canonical representation of Rationals
    val numer = x / gcdOfNonnegInts(x.abs, y)
    val denom = y / gcdOfNonnegInts(x.abs, y)

    // An extra benifit of dealing with pre-processed simplified representation of Rationals as opposed to only simplifying 
    // the Rationals in its external representation (via override of toString method), is that it mitigates the risk of integer 
    // overflow in the following method implementations
    def add(that: Rational): Rational = 
        new Rational(numer * that.denom + denom * that.numer, denom * that.denom)

    def subtract(that: Rational): Rational =
        new Rational(numer * that.denom - that.numer * denom, denom * that.denom)

    def multiply(that: Rational): Rational =
        new Rational(numer * that.numer, denom * that.denom)

    def divide(that: Rational): Rational =
        new Rational(numer * that.denom, denom * that.numer)

    def neg(): Rational =
        new Rational(-numer, denom)

    def less(that: Rational): Boolean =
        numer*that.denom < denom*that.numer

    def max(that: Rational): Rational =
        if (this.less(that)) that else this

    /*
    Override equals method 
    */
    private def equalRationals(that: Rational): Boolean =
        numer * that.denom == denom * that.numer

    override def equals(that: Any): Boolean = that match
        case that: Rational => equalRationals(that)
        case _ => false

    /* 
    Override toString method
    */
    override def toString(): String = 
        s"$numer/$denom"
end Rational

val x = Rational(1, 3)
val y = Rational(3, 2)
val z = Rational(9, 6)
val w = Rational(2, 3)
val r5 = Rational(0, 1)

// r5.gcd(0, 1)

assert(x.add(y) == Rational(11, 6))
assert(y.subtract(x) == Rational(7, 6))

assert(y.subtract(z) == Rational(0, 18))
assert(y==z)

assert(x.subtract(w)==Rational(-1, 3))