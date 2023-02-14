package context

import value._

object flags {
  val BY_REF = 0
  val BY_NAME = 1
  val BY_TEXT = 2
  var paramPassing = BY_REF
  var staticScoping = true
}
