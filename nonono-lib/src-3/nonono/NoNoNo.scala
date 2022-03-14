package nonono

sealed trait NoNoNo:
  def apply[T](f: T => Any)(cause: String): Nothing = ???

object NoNoNo extends NoNoNo
