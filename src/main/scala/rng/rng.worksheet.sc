// Linear congruential generator
class Generator(previous: Int):
    def next(): (Int, Generator) =
        val result = previous * 22_697_447 + 1
        (result, Generator(result))

    def between(x: Int, y: Int): (Int, Generator) =
        val min = math.min(x, y)
        val delta = math.abs(x - y)
        val (generatedVal, generator) = next()
        (generatedVal % delta + min, generator)

object Generator:
    def init: Int = 42

val gen = Generator(Generator.init)
val (rn1, gen2) = gen.between(0, 4)
val (rn2, gen3) = gen2.between(0, 4)
val (rn3, gen4) = gen3.between(0, 4)
gen4.next() // gen4 result returns negative