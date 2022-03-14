package nonono

sealed trait NoNoNo:
  def apply[T](prevent: T => Any)(hint: String): Nothing = ???

object NoNoNo extends NoNoNo
