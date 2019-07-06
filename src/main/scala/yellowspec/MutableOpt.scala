
package yellowspec

class MutableOpt[A] {
	private[this] var myValue: A = _
	private[this] var loaded = false

	private def valueEquals(o: Any) = myValue == o

	def get = if (loaded) myValue else throw new NoSuchElementException("MutableOpt")

	def set(a: A): this.type = {
		loaded = true; myValue = a; this
	}

	def getOrSet(a: => A): A = {
		if (!loaded) {
			myValue = a
			loaded = true
		}
		myValue
	}

	def isEmpty = !loaded

	def nonEmpty = loaded

	def foreach[U](f: A => U): Unit = if (loaded) f(myValue)

	def transform(f: A => A): this.type = {
		if (loaded) myValue = f(myValue); this
	}

	def clear(): this.type = {
		loaded = false; this
	}

	def toOption = if (loaded) Some(myValue) else None

	override def toString = if (loaded) "MutableOpt(" + myValue.toString + ")" else "MutableOpt()"

	override def hashCode = if (loaded) myValue.hashCode else 1751

	override def equals(o: Any) = o match {
		case m: MutableOpt[_] =>
			(isEmpty && m.isEmpty) || (nonEmpty && m.nonEmpty && m.valueEquals(myValue))
		case _ => false
	}
}

object MutableOpt {
	def from[A](o: Option[A]) = {
		val m = new MutableOpt[A]
		o match {
			case Some(a) => m set a
			case _ =>
		}
		m
	}
}