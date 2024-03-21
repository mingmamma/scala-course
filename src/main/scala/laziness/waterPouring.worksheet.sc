// model glasses involved with Int index
type Glass = Int
// model the state by a container Vector[Int] of Int index for each involved glass with amount of water in the respective glass
type State = Vector[Int]

// the parameter of State type representing the full capacities of available glass with a Vector[Int] fully characterise 
// the set up information of the pouring process by impling the following details: 
// the number of glasses involved in the set up
// the capacity of each glass involved
class PouringSetup(fullGlassCapcities: State, val goal: Int):
    
    enum Move:
        case EmptyGlass(glass: Glass)
        case FillGlass(glass: Glass)
        case Pour(fromGlass: Glass, toGlass: Glass)

        // define method associated to an enum s.t. it can be expected to be called as the following:
        // enumVariantInstance.methodName(methodParameters*) returning methodReturn type
        // where the enumVariantInstance type checks for the method call by subtyping the enumType
        // Specifically, calling execute on one variant of the Move enum with the provided parameter of State type
        // is meant to transform the state given by the parameter to return a new state as the effect of the Move variant
        def execute(state: State): State = this match
            // Noting the use of updated method of Vector type to for returning a Vector of the new state
            case EmptyGlass(glass) => state.updated(glass, 0)
            case FillGlass(glass) => state.updated(glass, fullGlassCapcities(glass))
            // the exact amount of pour is determined by the least of two quantities:
            // the amount of water available in the fromGlass
            // the amount of the full capacity less the current water amount of the toGlass
            case Pour(fromGlass, toGlass) => 
                val exactPourAmount: Int = state(fromGlass).min(fullGlassCapcities(toGlass)-state(toGlass))
                state.updated(fromGlass, state(fromGlass) - exactPourAmount)
                     .updated(toGlass, state(toGlass) + exactPourAmount)
    
    end Move

    // all possbile moves at any given point of the pouring process
    def moves: IndexedSeq[Move] =
        val glasses: Range = 0 until fullGlassCapcities.length
        glasses.map[Move]((glass: Glass) => Move.EmptyGlass(glass))
        ++ glasses.map[Move]((glass: Glass) => Move.FillGlass(glass))
        ++ glasses.flatMap[Move]((glassA: Glass) =>
            glasses.withFilter((glassB: Glass) => glassA != glassB).map[Move]((glassB: Glass) =>
                Move.Pour(glassA, glassB)
            )
        )

        // would-be equivalent implementation with for-comprehension given in lecture
        // (for glass <- glasses yield Moves.EmptyGlass(glass))
        // ++ (for glass <- glasses yield Moves.FillGlass(glass))
        // ++ (for glassA <- glasses; glassB <- glasses if glassA != glassB yield Moves.Pour(glassA, glassB))

    // model the outcome of pouring process as search through a path of moves by two parameters:
    // all the past moves leading to the current state
    // the current state configuration
    class searchPath(historyMoves: List[Move], val standingState: State):
        override def toString: String =
            historyMoves.reverse.mkString(",") + "->" + standingState.toString
        
        def extend(nextMove: Move): searchPath =
            val wouldBeHistoryMoves: List[Move] = nextMove :: historyMoves
            val wouldBeState = nextMove.execute(standingState)
            searchPath(wouldBeHistoryMoves, wouldBeState)

    // infinite lazy generator of all possible outcomes of search paths from given configurations
    // the generator is designed s.t. the each element returned by the LazyList at a time is a List[searchPath]
    // all of which have the same `distance` to the initial configuration
    // It allows for searching in a breath-first-search fashion s.t. the shortest searchPath to goal configuration
    // can be returned once found available 
    def allPaths(thisLevelPaths: List[searchPath], exploredStates: Set[State]): LazyList[List[searchPath]] =
        
        val nextLevelPaths: List[searchPath] =
            thisLevelPaths.flatMap[searchPath]((existingPath: searchPath) =>
                moves.withFilter(move => !exploredStates(move.execute(existingPath.standingState)))
                     .map[searchPath](move => existingPath.extend(move))
            )

        val wouldBeExploredStates = exploredStates ++ nextLevelPaths.map(path => path.standingState).toSet

        thisLevelPaths #:: allPaths(nextLevelPaths, wouldBeExploredStates)

    // solution finding implementation by imperative style s.t. the termination condition on search outcome
    // is explicitly implemented to return search result, or otherwise search solution further recursively
    def returnSolution(lazyPathsGenerator: LazyList[List[searchPath]]): List[String] =
        if lazyPathsGenerator.head.exists((pathToGoal: searchPath) =>
            pathToGoal.standingState.contains(goal)
        ) then lazyPathsGenerator.head.filter(pathToGoal =>
           pathToGoal.standingState.contains(goal)
        ).map(path => path.toString)
        else returnSolution(lazyPathsGenerator.tail)

    // a more functional style implementation for solution finding is to return a infinitely generator of LazyList of solutions
    // s.t. the determination of stopping at some solution is decoupled and left for the user
    def returnSolution2(startingPaths: List[searchPath], startingExploredStates: Set[State]): LazyList[searchPath] =
        allPaths(startingPaths, startingExploredStates)
            .flatMap[searchPath]((pathsOfSomeDistance: List[searchPath]) => 
                pathsOfSomeDistance.withFilter((path: searchPath) => 
                    path.standingState.contains(goal)).map[searchPath](identity)
            )
    // Equivalent implementation of lazily returning solution with for comprehension given in lecture
    def returnSolution3(startingPaths: List[searchPath], startingExploredStates: Set[State]): LazyList[searchPath] =
        for
            pathsOfSomeDistance <- allPaths(startingPaths, startingExploredStates)
            path <- pathsOfSomeDistance
            if path.standingState.contains(goal)
        yield
            path
        

end PouringSetup

val pouringA = PouringSetup(Vector(4, 7), 6)
val pouringAStartConfig = pouringA.searchPath(Nil, Vector(0, 0))
val pouringAPaths = pouringA.allPaths(List(pouringAStartConfig), Set.empty)
pouringA.returnSolution(pouringAPaths)
pouringA.returnSolution2(List(pouringAStartConfig), Set.empty).head
pouringA.returnSolution3(List(pouringAStartConfig), Set.empty).head

        