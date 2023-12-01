file://<WORKSPACE>/src/main/scala/example/example.worksheet.sc
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

action parameters:
offset: 3179
uri: file://<WORKSPACE>/src/main/scala/example/example.worksheet.sc
text:
```scala
object worksheet{
  import scala.collection.mutable
  
  1 + 1
  
  val x = 42
  
  x * x
  
  1 + 2.0
  
  val facade: Int = 5 * 3
  
  // 1 / 0
  
  // 1 && "Sound and Vision"
  
  def house(facade: Double, window: Double): Double = 
      val door = 2 * 1
      val substracted = door + window*2
      facade - substracted
  end house
  
  house(5*3, 1*1)
  
  def marathonDuration(speed: Double): Double =
      val distance = 42.195
      val duration = distance / speed
      duration*60
  end marathonDuration
  
  marathonDuration(12)
  
  def showPrice(paintingArea: Double, paintPrice: Double): String =
      val price = paintingArea * paintPrice
      if price > 100 then
          "too expensive"
      else if price < 10 then
          "very cheap"
      else
          price.toString
  
  // immutable data type
  // case class Circle(radius: Int):
  //     val area = radius * radius * 3.14
  
  sealed trait Shape
  case class Rectangle(width:Int, height: Int) extends Shape
  case class Circle(radius:Int) extends Shape
  
  def calculateArea(shape: Shape): Double =
      shape match
      case Rectangle(width,height) => width*height
      case Circle(radius) => radius*radius*3.14
  
  val someRectangle: Rectangle = Rectangle(4,5)
  val someShape:Shape = someRectangle
  val area = calculateArea(someRectangle)
  
  // check if a three cards forms a set
  // https://en.wikipedia.org/wiki/Set_(card_game)
  case class Card(shaping:Shaping,number:Number,color:Color,shading:Shading)
  
  sealed trait Prop
      enum Color extends Prop:
          case red,green,purple
      enum Number extends Prop:
          case `1`,`2`,`3`
      enum Shaping extends Prop:
          case diamond,oval,squiggle
      enum Shading extends Prop:
          case stripped,open,solid
  
  def checkProperty(cardOneProp:Prop, cardTwoProp:Prop, cardThreeProp:Prop): Boolean=
      def allSame = (cardOneProp==cardTwoProp)&&(cardTwoProp==cardThreeProp)
      def allDiff = (cardOneProp!=cardTwoProp)&&(cardTwoProp!=cardThreeProp)&&(cardOneProp!=cardThreeProp)
      allSame || allDiff       
  
  def isSet(cardOne:Card, cardTwo:Card, cardThree:Card): Boolean=
      checkProperty(cardOne.color,cardTwo.color,cardThree.color)&&
      checkProperty(cardOne.number,cardTwo.number,cardThree.number)&&
      checkProperty(cardOne.shaping,cardTwo.shaping,cardThree.shaping)&&
      checkProperty(cardOne.shading,cardTwo.shading,cardThree.shading)
  
  val card1=Card(Shaping.diamond,Number.`1`,Color.purple,Shading.stripped)    
  val card2=Card(Shaping.squiggle,Number.`2`,Color.red,Shading.open)    
  val card3=Card(Shaping.oval,Number.`3`,Color.green,Shading.solid)
  // not part of the set
  val card4=Card(Shaping.oval,Number.`3`,Color.green,Shading.stripped)
  
  assert(isSet(cardOne = card1, cardTwo = card2, cardThree = card3 )==true)
  assert(isSet(cardOne = card1, cardTwo = card2, cardThree = card4 )==false)
  
  // flat map example
  mutable.ArrayBuffer(1,2).flatMap(x=>mutable.ArrayBuffer(x,x*2))
  
  // group by example
  val emails=List("alice@sca.la","bob@sca.la","carol@earth.world")
  
  // function for extracting domain name from an email address
  val domain(@@): String=>String = email.dropWhile(c=>c!='@').drop(1)
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