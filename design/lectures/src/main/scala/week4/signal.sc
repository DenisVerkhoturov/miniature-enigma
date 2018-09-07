class Signal[T](expr: => T) {
  def apply(): T = ???
  def update(expr: => T): Unit = ???
}

object Signal {
  def apply[T](expr: => T): Signal[T] = new Signal(expr)
}

class Var[T](expr: => T) extends Signal[T](expr) {
  override def update(expr: => T): Unit = super.update(expr)
}

object Var {
  def apply[T](expr: => T): Var[T] = new Var(expr)
}

class BankAccount {
  private val balance = Var(0)
  def deposit(amount: Int): Unit = {
    if (amount > 0) {
      val b = balance()
      balance() = b + amount
    }
  }
  def withdraw(amount: Int): Int = {
    if (0 < amount && amount <= balance()) {
      val b = balance()
      balance() = b - amount
    }
    else throw new Error("insufficient funds")
    balance
  }
}
