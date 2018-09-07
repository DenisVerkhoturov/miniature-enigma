abstract class Account(private[Account] var amount: Int = 0) {
  def transfer(target: Account, amount: Int): Unit
}

class DeadlockAccount(private var amount: Int = 0) extends Account(amount) {
  def transfer(target: Account, amount: Int): Unit = this synchronized { target synchronized {
    this.amount -= amount
    target.amount += amount
  } }
}

def startThread(left: Account, right: Account, amount: Int) = {
  val thread = new Thread {
    override def run() = for (_ <- 0 until amount) left.transfer(right, 1)
  }
  thread.start()
  thread
}

val a1 = new DeadlockAccount(500000)
val a2 = new DeadlockAccount(700000)

val t1 = startThread(a1, a2, 150000)
val t2 = startThread(a2, a1, 150000)
t1.join()
t2.join()
