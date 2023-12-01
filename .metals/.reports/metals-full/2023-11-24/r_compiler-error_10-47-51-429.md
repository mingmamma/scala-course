file://<WORKSPACE>/src/main/scala/example/example.worksheet.sc
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

action parameters:
offset: 1304
uri: file://<WORKSPACE>/src/main/scala/example/example.worksheet.sc
text:
```scala
object worksheet{
  import java.awt.Shape
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
  val calculateArea(@@)
  
  
  
  sealed trait Notification
  
  case class Email(sender: String, title: String, body: String) extends Notification
  
  case class SMS(caller: String, message: String) extends Notification
  
  case class VoiceRecording(contactName: String, link: String) extends Notification    
  def showNotification(notification: Notification): String =
    notification match
      case Email(sender, title, _) =>
        s"You got an email from $sender with title: $title"
      case SMS(number, message) =>
        s"You got an SMS from $number! Message: $message"
      case VoiceRecording(name, link) =>
        s"You received a Voice Recording from $name! Click the link to hear it: $link"    
  
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