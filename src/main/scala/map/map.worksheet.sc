// Design of the polynomial class with the idea that
// its coefficients of various degress can be expressed by a Map
class Polynom(val nonZeroTerms: Map[Int, Double]):

    // secondary constructor to facilitate Polynom 
    // construct syntax without explicit Map
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    // a map from degree to coefficient, with 0 coefficient for missing terms,
    // representing the polynomial
    val terms = nonZeroTerms.withDefaultValue(0.0)

    def + (other: Polynom): Polynom =
        val sumNonZeroTerms = nonZeroTerms ++ other.nonZeroTerms.map((deg, coef) =>
            // update the coef if keys intersect
            if nonZeroTerms.contains(deg) then (deg, coef + nonZeroTerms(deg))
            // other update/add other deg & coef
            else (deg, coef))
        Polynom(sumNonZeroTerms)

    override def toString(): String =
        // val termsList = nonZeroTerms.map {case (deg, coef) => 
        //     if deg > 1 then "%.3f * x^%d".format(coef,deg)
        //     else if deg == 1 then "%.3f * x".format(coef)
        //     else "%.3f".format(coef)}.toList
        // termsList.mkString(" + ")
        
        // lecturer implmentation
        if nonZeroTerms.isEmpty then "0"
        else {
            val termsList =
                for (deg, coef) <- nonZeroTerms.toList.sorted.reverse
                yield
                    val expo = s"x^$deg"
                    s"$coef * $expo"
        
            termsList.mkString(" + ")
        }





val poly1 = Polynom(0 -> 2, 1 -> 3, 2 -> 1)

val poly0 = Polynom(Map())

val poly1Inv = Polynom(0 -> -2, 1 -> -3, 2 -> -1)

val sumPoly1_0 = poly0 + poly1

val sumPoly1WithInv = poly1 + poly1Inv

val doublePoly1 = poly1 + poly1
