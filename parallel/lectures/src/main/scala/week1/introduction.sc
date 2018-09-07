val lock = new AnyRef{}
var uidCount = 0L

def getUniqueId(): Long = lock.synchronized {
    uidCount = uidCount + 1
    uidCount
  }

def startThread(): Thread = {
  val thread = new Thread {
    override def run(): Unit = {
      val uids = for (_ <- 0 until 10) yield getUniqueId()
      println(uids)
    }
  }

  thread.start()
  thread
}

val t1 = startThread()
val t2 = startThread()
t1.join()
t2.join()
