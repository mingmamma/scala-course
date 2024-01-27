// example logarithm
class Logarithm(protected val underlying: Double):
  def toDouble: Double = math.exp(underlying)
  def + (that: Logarithm): Logarithm =
    // here we use the apply method on the companion
    Logarithm(this.toDouble + that.toDouble)
  def * (that: Logarithm): Logarithm =
    // here we use the class Logarithm constructor
    new Logarithm(this.underlying + that.underlying)

object Logarithm:
  def apply(d: Double): Logarithm = new Logarithm(math.log(d))

val l2 = Logarithm(2.0)
val l3 = Logarithm(3.0)

println((l2 * l3).toDouble)
println((l2 + l3).toDouble)