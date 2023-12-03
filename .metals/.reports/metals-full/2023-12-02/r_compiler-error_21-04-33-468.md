file://<WORKSPACE>/src/main/scala/example/example2.worksheet.sc
### dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition <error> is defined in
  <WORKSPACE>/src/main/scala/example/example2.worksheet.sc
and also in
  <WORKSPACE>/src/main/scala/example/example2.worksheet.sc
One of these files should be removed from the classpath.

occurred in the presentation compiler.

action parameters:
offset: 2684
uri: file://<WORKSPACE>/src/main/scala/example/example2.worksheet.sc
text:
```scala
object worksheet{
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
  
  // recusive split that takes a sequence of digits as a parameter 
  // and returns a collection listing all the ways to split that 
  // sequence of digits into sub-sequences of digits
  def digitPhrase(digits: String): Seq[Seq[String]] =
    if digits.isEmpty() then Seq(Nil)
    else 
      for
        splitPoint <- 1 to digits.length
        // str.splitAt(n) == (str.take(n), str.drop(n))
        (firstDigits, remainingDigits) = digits.splitAt(splitPoint)
        word <- index.getOrElse(firstDigits, Nil)
        words <- digitPhrase(remainingDigits)
      yield word +: words
  
  val index: Map[String, Set[String]] = Map(
    "63" -> Set("of", "me"),
    "66" -> Set("on", "no")
  )
  
  digitPhrase("63")
  
  // digit to letter group
  val keys: Map[Char, String] = Map(
    '2' -> "ABC",
    '3' -> "DEF",
    '4' -> "GHI",
    '5' -> "JKL",
    '6' -> "MNO",
    '7' -> "PQRS",
    '8' -> "TUV",
    '9' -> "WXYZ"
  )
  
  // generate letter to digit
  val letterToDigit: Map[Char, Char] =
    for 
      (digit, letterGroup) <- keys
      letter <- letterGroup
    yield letter -> digit
  
  def wordToDigits(word: String): String = 
    word.toUpperCase.map(letterToDigit)
  
  class Mnemonics(dictionary: Set[String])
    def of@@
}
```



#### Error stacktrace:

```

```
#### Short summary: 

dotty.tools.dotc.core.TypeError$$anon$1: Toplevel definition <error> is defined in
  <WORKSPACE>/src/main/scala/example/example2.worksheet.sc
and also in
  <WORKSPACE>/src/main/scala/example/example2.worksheet.sc
One of these files should be removed from the classpath.