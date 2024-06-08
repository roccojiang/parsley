package parsley.garnish.utils

class Fresh {
  private val id = new java.util.concurrent.atomic.AtomicInteger(1)

  def next(): String = next("x")
  def next(prefix: String): String = prefix + id.getAndIncrement()
}
