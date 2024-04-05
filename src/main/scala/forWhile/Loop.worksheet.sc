import scala.compiletime.ops.double
// raise a double number to a given nonnegative integer power
def nonNegPower(x: Double, power: Int): Double =
    require(power >= 0, "expecting nonnegative integer power to raise to")
    var xRaised: Double = 1.0
    var powerRaised: Int = 0
    // use while-do control construct for the imperative implementation
    while powerRaised < power do {xRaised = xRaised * x; powerRaised = powerRaised + 1;}
    xRaised

// Emulate the while-do control construct with a function
def whileDo(condition: => Boolean)(body: => Unit): Unit =
    if (condition == true) then {body; whileDo(condition)(body)}
    // optionally explicit return on else case
    // else ()

def nonNegPower2(x: Double, power: Int): Double =
    var xRaised: Double = 1.0
    var powerRemained: Int = power
    whileDo(powerRemained > 0){xRaised = xRaised * x; powerRemained = powerRemained - 1;}
    xRaised

nonNegPower(2, 0)
nonNegPower(2, 1)
nonNegPower(2, 2)
nonNegPower2(2, 0)
nonNegPower(2, 1)
nonNegPower2(2, 2)

// the relation of the two control construct while-do and do-while on the same inputs (of condition and body) is given by
// do (body) while (condition)
// BEING EQUIVALENT TO    
// {
//     body
//     while (condition) do (body)
// }
// the differentiating characteristics of the while-do and do-while is that while-do executes the body 0 or more time
// whereas while-do executes the body 1 or more times, i.e. at least 1 time


// rasie a double number to a given positive power s.t the return cannot be 1 unless the base double was 1
def posPower(x: Double, power: Int): Double =
    
    require(power > 0, "expecting positive integer power to raise to")
    
    var xRaised: Double = 1.0
    var powerRaised: Int = 0
    
    // https://docs.scala-lang.org/scala3/guides/migration/incompat-dropped-features.html#do-while-construct
    // do (body) while (condition) is supported in Scala 2 only. Thus the the workaround shows the equivalent effect of tranlation from
    // a do...While... to while...do... with identical condition and body inputs
    // do {xRaised = xRaised * x; powerRaised = powerRaised + 1;} while powerRaised < power
    
    {xRaised = xRaised * x; powerRaised = powerRaised + 1;}
    while powerRaised < power do {xRaised = xRaised * x; powerRaised = powerRaised + 1;}
    xRaised

// Emulate the do-while construct with a function
def doWhile(condition: => Boolean)(body: => Unit): Unit =
    body;
    if (condition == true) then doWhile(condition)(body) else ()

def posPower2(x: Double, power: Int): Double =
    require(power > 0, "expecting positive integer power to raise to")
    var xRaised: Double = 1.0
    var powerRaised: Int = 0
    doWhile(powerRaised < power){xRaised = xRaised * x; powerRaised = powerRaised + 1;}
    xRaised

// A more realisic emulation to the exact syntax of the do-while construct?!

posPower(2, 1)
posPower(2, 2)
posPower2(2, 1)
posPower2(2, 2)

// https://docs.scala-lang.org/scala3/book/control-structures.html#for-loops
// Scala also provides a for-loop construct, e.g.
for 
    num <- 1 to 5 by 2
    char <- "ab"
do 
    val doubleNum = num * 2
    println(s"num = $num, doubledNum = $doubleNum, char = $char")

// for-loop is desugared to calls to the higher-of-function foreach, i.e. the equivalent rewrite would be
// paratheses () can ONLY enclosed a specific class of expression called "simple expressions", as opposed to curly brackets {}?!
// https://www.scala-lang.org/files/archive/spec/2.13/06-expressions.html#for-comprehensions-and-for-loops
(1 to 5 by 2).foreach{case num: Int =>
    "ab".foreach{case char: Char =>
        {
            val doubleNum = num * 2
            println(s"num = $num, doubledNum = $doubleNum, char = $char")
        }
    }
}


