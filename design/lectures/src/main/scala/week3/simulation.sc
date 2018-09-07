class Inv

class And

class Or

class Wire {
  private var sigVal = false
  private var actions: List[Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(signal: Boolean): Unit = if (signal != sigVal) {
    sigVal = signal
    actions foreach (_ ())
  }

  def addAction(a: Action): Unit = {
    actions = a :: actions
    a()
  }
}

type Action = () => Unit

trait Simulation {
  case class Event(time: Int, action: Action)
  private type Agenda = List[Event]
  private var agenda: Agenda = List()
  private var curtime = 0

  def currentTime: Int = curtime

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  def run(): Unit = {
    afterDelay(0) {
      println(s"*** simulation started, time = $currentTime ***")
    }
    loop()
  }

  private def insert(agenda: Agenda, item: Event): Agenda = agenda match {
    case first :: rest if first.time <= item.time => first :: insert(rest, item)
    case _=> item :: agenda
  }

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case Nil =>
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime value = ${wire.getSignal}")
    }
    wire addAction probeAction
  }
}

trait Parameters {
  def InverterDelay = 2
  def AndGateDelay = 3
  def OrGateDelay = 5
}

abstract class Gates extends Simulation {

  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSignal = input.getSignal
      afterDelay(InverterDelay) {
        output setSignal !inputSignal
      }
    }
    input addAction invertAction
  }

  def andGate(leftInput: Wire, rightInput: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val leftSignal = leftInput.getSignal
      val rightSignal = rightInput.getSignal
      afterDelay(AndGateDelay) {
        output setSignal (leftSignal & rightSignal)
      }
    }
    leftInput addAction andAction
    rightInput addAction andAction
  }

  def orGate(leftInput: Wire, rightInput: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val leftSignal = leftInput.getSignal
      val rightSignal = rightInput.getSignal
      afterDelay(OrGateDelay) {
        output setSignal (leftSignal | rightSignal)
      }
    }
    leftInput addAction orAction
    rightInput addAction orAction
  }
}

abstract class Circuits extends Gates {
  /**
    * {{{
    * a >━ ┳ ━ ━ ┏━━━━━┓
    *      ┃     ┃ OR ┃ ━ ━ ━ ━ ━ ━ ━ ━ ━ ┏━━━━━━┓
    *      ┃ ┏ ━ ┗━━━━━┛        ┏━━━━━━┓   ┃ AND ┃ ━ ━ ━> s
    *      ┃ ┃             ┏ ━ ┃ INV ┃ ━ ┗━━━━━━┛
    *      ┗ ━ ━ ┏━━━━━━┓   ┃   ┗━━━━━━┛
    *        ┃   ┃ AND ┃ ━ ┻ ━ ━ ━ ━ ━ ━ ━ ━ ━ ━ ━ ━ ━ ━> c
    * b >━ ━ ┻ ━ ┗━━━━━━┛
    * }}}
    */
  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    val d, e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  /**
    * {{{
    * a >━ ━ ━ ━ ━ ━ ━ ┏━━━━━┓ ━ ━ ━ ━ ━ ━ ━ ━> sum
    *                 ┃ HA ┃
    * b >━ ━ ┏━━━━━┓ ━ ┗━━━━━┛ ━ ┏━━━━━┓
    *        ┃ HA ┃            ┃ OR ┃  ━ ━ ━> cout
    * cin >━ ┗━━━━━┛ ━ ━ ━ ━ ━ ━ ┗━━━━━┛
    * }}}
    */
  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    val s, c1, c2 = new Wire
    halfAdder(b, cin, s, c1)
    halfAdder(a, s, sum, c2)
    orGate(c1, c2, cout)
  }
}

object sim extends Circuits with Parameters
import sim._
val in1, in2, sum, carry = new Wire
halfAdder(in1, in2, sum, carry)
probe("sum", sum)
probe("carry", carry)

in1 setSignal true
run()
in2 setSignal true
run()
in1 setSignal false
run()
