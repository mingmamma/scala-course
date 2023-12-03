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

class Mnemonics(dictionary: Set[String]):

  private val phoneKeys: Map[Char, String] = Map(
    '2' -> "ABC",
    '3' -> "DEF",
    '4' -> "GHI",
    '5' -> "JKL",
    '6' -> "MNO",
    '7' -> "PQRS",
    '8' -> "TUV",
    '9' -> "WXYZ"
  )

  private val letterToDigit: Map[Char, Char] =
    for
      (digit, letters) <- phoneKeys
      letter           <- letters
    yield letter -> digit

  private def wordToDigits(word: String): String =
    word.toUpperCase.map(letterToDigit)
  
  private val wordsByDigits: Map[String, Set[String]] = 
    dictionary.groupBy(wordToDigits)
  
// recusive split that takes a sequence of digits as a parameter 
// and returns a collection listing all the ways to split that 
// sequence of digits into sub-sequences of digits  
  def ofPhoneNumber(digits: String): Seq[Seq[String]] = 
    if digits.isEmpty() then Seq(Nil)
    else 
      for
        splitPoint <- 1 to digits.length
        // str.splitAt(n) == (str.take(n), str.drop(n))
        (firstDigits, remainingDigits) = digits.splitAt(splitPoint)
        word <- wordsByDigits.getOrElse(firstDigits, Nil)
        words <- ofPhoneNumber(remainingDigits)
      yield word +: words
end Mnemonics

val dictionary = Set("of", "me", "on", "no")
val mnemonics = Mnemonics(dictionary)
mnemonics.ofPhoneNumber("63")
mnemonics.ofPhoneNumber("6366")