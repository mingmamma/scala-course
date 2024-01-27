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