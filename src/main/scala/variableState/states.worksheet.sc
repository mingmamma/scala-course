// Consider a publish-subscriber model as following. Consider some bank accounts whose states change over time by actions of deposits and withdraws.
// If we were to track the total balance of all involved account over time, then an observer pattern would be applicable.
// In such a setting, bank accounts are can be modelled by the publisher, and the account consolidator can be modelled by the subscriber.
// Whenever the states of publishers change, the notification of such changes are communicated to subscribers so that the subscriber can vary accordingly.

// the Publisher trait are implemented s.t
// On creation of a publisher instance, its subscribers are to be maintained in a set, starting from an empty set
// Public interfaces are provided (to potential subscribers) so that they can subscribe to and unsubscribe from the publisher
// Private interface are implemented to publish future changes of its state to its subscribers, fullfiled by subscribers handling of the published notification
trait Publisher:
    private var subscribers: Set[Subscriber] = Set.empty

    def subscribe(newSubscriber: Subscriber): Unit =
        subscribers = subscribers + newSubscriber
        // subscribers = subscribers.incl(newSubscriber)

    def unsubscribe(goingSubscriber: Subscriber): Unit =
        subscribers = subscribers - goingSubscriber
        // subscribers = subscribers.excl(goingSubscriber)

    // it seems declaring a private member in a trait is a always a dead-end?!
    protected def publish(): Unit =
        subscribers.foreach((subscriber: Subscriber) =>
            subscriber.handleNotification(this)
        )

trait Subscriber:

    def handleNotification(notificationPublisher: Publisher): Unit

// Modelling a bank account whose states vary over time
// class BankAccount():
class BankAccount extends Publisher:
    private var balance: Int = 0

    // provides a public interface to query the current state of balance
    def currentBalance = balance

    def deposit(amount: Int): Unit =
        require(amount > 0, "Deposit expects a postive amount")
        balance = balance + amount
        publish()

    def withdraw(amount: Int): Unit =
        require(amount <= balance && amount > 0, "Withdraw expects a postive amount not over the current balance")
        balance = balance - amount
        publish()


// Modelling an account consolidator whose role is to public a view into the total balance of all involved bank accounts at any given time
class AccountConsolidater(bankAccounts: List[BankAccount]) extends Subscriber:
    // the initialization of the account consolidator takes a list of bank accounts as parameter s.t.
    // the subsciption relation is established on the creation of the account consolidator
    // Assuming the account consolidator only provides limited capacity s.t. the list of bank accounts are given and fixed
    // on the creation of the account consolidator, without facility to be updated later
    bankAccounts.foreach((account: BankAccount) =>
        account.subscribe(this)
    )

    // reference to this syntax of "delayed initialization"?!
    // this is meant to delegate the initilization of totalBalance value to the computeTotalBalance call within which it is fulfilled
    private var totalBalance: Int = _
    computeTotalBalance()

    // provides a public interface for callers of account consolidator to query the total balance
    def totalBalanceView(): Int = totalBalance

    def computeTotalBalance(): Unit =
        totalBalance = bankAccounts.map[Int]((account: BankAccount) =>
            account.currentBalance).sum[Int]
    
    // Handle an state change from a bank account, in the context of account consolidator, is to update its view of the total balance
    def handleNotification(notificationPublisher: Publisher): Unit = computeTotalBalance()

val accountA = BankAccount()
val accountB = BankAccount()
val tracker = AccountConsolidater(List(accountA, accountB))
accountA.deposit(30)
accountB.deposit(20)
tracker.totalBalanceView()
accountA.withdraw(30)
tracker.totalBalanceView()
// Would throw IllegalArgument exception
// accountB.withdraw(30)

// Implementation of functional reactive programming utility
object frp

// abstract Signal trait that provides the most basic interface of Signal
// which is a generic that that can return a value of the type on application
trait Signal[+T]:
    def apply(): T

object Signal:
    // Observer is the type alias
    // the scope of opaque type definition is the Signal object definition scope
    opaque type Observer = AbstractSignal[?]
    def caller: Observer = ???

    // A more concrete representation of Signal that will encompass two types of Signals, i.e. a constant signal whose
    // evaluation formula is given & fixed on creation, or a variable signal whose evaluation formula can be updated
    // The modelling of Signal at this level have three abstract members, i.e. the current value of the signal, the 
    // evaluation formula of the signal, and the known observers dependent on this signal s.t. they need to be notified
    // of the change in the currentValue of this signal
    abstract class AbstractSignal[+T] extends Signal[T]:
        // For either of the two Signal types, initialization of a Signal would set the currentValue unitialized (handled otherwise)
        private var currentValue: T = _
        
        // For either of the two Signal types, initialization of a Signal would safely assume it to have NO observer signal yet
        private var previousObservers: Set[Observer] = Set()
        
        // abstract member for the evaluation formula of the signal
        // given this member would have concrete implementation in the two concrete Signal subclasses, the `protected` keyword
        // keeps the accessibility of the member to minimum, reserving its use in subclasses. `private` keyword is disallowed by compiler
        // on abstract member as that would forbid eventual concrete implementation
        protected def evaluationFormula: () => T

        // the dynamic updates to observers is maintained in the cycle of getting the currentValue on Signal application and the action of
        // setting currentValue on evaluation the evaluationFormula and propagate changed currentValue to dependent observers

        // an anplication of the Signal entails its currentValue is inquired, which is what the method returns
        // it also entails the Observer calls inquiring the currentValue is dependent on this Signal s.t. the caller
        // is added to the observers set to maintain the dependency
        def apply(): T =
            previousObservers = previousObservers + caller
            currentValue

        // evaluating the evaluationFormula is responsible for: updating the currentValue by the given evaluationFormula and
        // last but not least, if the currentValue had change, propagate that to the dependent observers by demanding those to
        // evaluating by their current evaluationFormula
        
        // to complete the observer dependency maintainance cycle, dependent observers would be cleared from the observers set given that
        // they would have the observed Signals adding them back to the observers set for dependency relationship, based on their current
        // evaluationFormula, by invoking the evaluationFormula which would involve inquirying observed Signals

        // summing up all the use cases of evaluateAndPropagate, we see three classes of uses: the initialization of any subclass instance of
        // AbstractSignal which initializes the currentValue, the update of evaluationFormula needs to be completed with evaluateAndPropogate
        // to take effect. In these two cases, the call is initiated by the instance itself. Lastly, as part of evaluateAndPropagate of an instance,
        // it would need to notify its known observers by demanding evaluateAndProprogate call in those observers
        def evaluateAndPropagate(): Unit =
            val updatedValue: T = evaluationFormula();
            currentValue = updatedValue
            // Only propagate for actual change in currentValue
            val changedObserved: Boolean = (updatedValue != currentValue) && previousObservers.nonEmpty
            if changedObserved then
                val observers = previousObservers
                previousObservers = Set()
                observers.foreach((observer: Observer) =>
                    observer.evaluateAndPropagate()
                )

    end AbstractSignal

    // factory method for the creation of the "regular" constant Signal whose evaluation formula is fixed
    // the implementation is to return an anonymous subclass of AbstractSignal
    def apply[T](formula: => T): Signal[T] =
        new AbstractSignal[T] {
            val evaluationFormula = () => formula
            // the effect of evaluateAndProprogate as the last step in the initilization of the constant Signal would only
            // limit to initilize the currentValue as there would be no member in the observers set to propagate to
            evaluateAndPropagate()
        }
    
    // class definition and constructor method of the "variable" Signal whose evaluation formula is allowed to
    // change over time
    class Var[T](initFormula: => T) extends AbstractSignal[T]:
        // it seems that overriding definition in subclass is NOT allowed to carry `private` modifier?!
        protected var evaluationFormula = () => initFormula
        // for completing the subclass instance initialization
        evaluateAndPropagate()

        // public interface to accepting and updateing the evaluation formula
        def update(newFormula: => T): Unit =
            evaluationFormula = () => newFormula
            evaluateAndPropagate()

    end Var
    





