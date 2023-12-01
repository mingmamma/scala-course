// example opaque type
object Lengths:
  opaque type Meters = Double
  def Meters(value: Double): Meters = value
  def add(x: Meters, y: Meters): Meters = x + y
  def show(x: Meters): String = s"$x m"

def usage(): Unit =
  import Lengths.*
  val twoMeters: Meters = Meters(2.0)
//   val fourMeters1: Double = twoMeters + twoMeters // Invalid
//   println(show(fourMeters1))
//   val fourMeters2: Double = add(twoMeters, twoMeters) // Invalid
  val fourMeters3: Meters = add(twoMeters, twoMeters)
  println(show(fourMeters3))

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

// example to extend int operation
extension (n: Int)
    // define n to the power of e, by filling the list with n, e times, and evaluating the product
    def **(e: Int): Int = List.fill(e)(n).product

5**3
**(5)(3)