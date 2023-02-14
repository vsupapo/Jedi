package value

class Notification(val message: String) extends Value {
  override def toString: String = message
}

object Notification {
  def apply(msg: String) = new Notification(msg)
  val OK = Notification("ok")
  val DONE = Notification("done")
  val UNSPECIFIED = Notification("unspecified")
}