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