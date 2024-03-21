abstract class IntSet:
    def contains(i: Int): Boolean
    def include(i: Int): IntSet

object Empty extends IntSet:
    def contains(i: Int): Boolean =
        false

    def include(i: Int): IntSet =
        NonEmpty(Empty, Empty, i)

class NonEmpty(val left: IntSet, val right: IntSet, val ele: Int) extends IntSet:
    def contains(i: Int): Boolean =
        if i < ele then left.contains(i)
        else if i > ele then right.contains(i)
        else true

    def include(i: Int): IntSet =
        if i < ele then NonEmpty(left.include(i), right, ele)
        else if i > ele then NonEmpty(left, right.include(i), ele)
        else this

val empty_set = Empty
val single_val_set = NonEmpty(Empty, Empty, 1)

assert(empty_set.include(1).contains(1))
assert(single_val_set.contains(1))