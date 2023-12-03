file://<WORKSPACE>/src/main/scala/example/example2.worksheet.sc
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

action parameters:
offset: 2424
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
  val letterToDigit(@@)
}
```



#### Error stacktrace:

```
dotty.tools.dotc.core.SymDenotations$NoDenotation$.owner(SymDenotations.scala:2582)
	scala.meta.internal.pc.SignatureHelpProvider$.isValid(SignatureHelpProvider.scala:83)
	scala.meta.internal.pc.SignatureHelpProvider$.notCurrentApply(SignatureHelpProvider.scala:92)
	scala.meta.internal.pc.SignatureHelpProvider$.$anonfun$1(SignatureHelpProvider.scala:48)
	scala.collection.StrictOptimizedLinearSeqOps.loop$3(LinearSeq.scala:280)
	scala.collection.StrictOptimizedLinearSeqOps.dropWhile(LinearSeq.scala:282)
	scala.collection.StrictOptimizedLinearSeqOps.dropWhile$(LinearSeq.scala:278)
	scala.collection.immutable.List.dropWhile(List.scala:79)
	scala.meta.internal.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:48)
	scala.meta.internal.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:375)
```
#### Short summary: 

java.lang.AssertionError: NoDenotation.owner