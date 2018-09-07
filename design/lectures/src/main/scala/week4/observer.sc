import scala.collection.mutable

class BankAccount extends mutable.Publisher[Any] {
  private var balance = 0
  def currentBalance: Int = balance
  def deposit(amount: Int): Unit = {
    if (amount > 0) balance = balance + amount
    publish()
  }
  def withdraw(amount: Int): Int = {
    if (0 < amount && amount <= balance) {
      balance = balance - amount
    } else throw new Error("insufficient funds")
    publish()
    balance
  }
}

class Consolidator(observed: List[BankAccount]) extends mutable.Subscriber[Any, Any] {
  observed.foreach(_.subscribe(this))

  private var total: Int = _
  compute()

  def totalBalance: Int = total

  private def compute(): Unit = total = observed.map(_.currentBalance).sum

  override def notify(pub: Any, event: Any): Unit = compute()
}

val a = new BankAccount
val b = new BankAccount
val c = new Consolidator(a :: b :: Nil)

c.totalBalance
a deposit 20
c.totalBalance
b deposit 30
c.totalBalance
