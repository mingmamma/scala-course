// Structural recursion example, find the total completion time given task dependenc list
case class Task(name: String, duration : Int, requirements: List[Task])

val setup = Task("setup", 4, Nil)
val ide = Task("IDE", 3, Nil)
val install = Task("install", 5, Nil)
val code = Task("code", 8, List(setup,ide,install))
val deploy = Task("deploy", 3, List(code))

def maxRequirementsDuration(tasks: List[Task]): Int =
    tasks match
        case Nil => 0
        case head::tail =>
            val headDuration = totalDuration(head)
            val tailDuration = maxRequirementsDuration(tail)
            if headDuration > tailDuration then headDuration
            else tailDuration

def totalDuration(task : Task): Int =
    task.duration + maxRequirementsDuration(task.requirements)

totalDuration(deploy)

// another implementation
def totalDuration2(task : Task): Int = 
    val requirementsDuration = 
        task.requirements
            .map(totalDuration2)
            .maxOption
            .getOrElse(0)
    task.duration + requirementsDuration

totalDuration(deploy)