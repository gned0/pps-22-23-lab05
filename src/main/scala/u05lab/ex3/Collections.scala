package u05lab.ex3

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
  }

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

@main def checkPerformance(): Unit = {

  import PerformanceUtils.*

  println("Performance Measurement of Different Collections")
  println("=" * 60)

  /* Linear sequences: List, ListBuffer */
  val immutableList = (1 to 1_000_000).toList
  val mutableList = mutable.ListBuffer.empty[Int]
  mutableList ++= (1 to 1_000_000)

  println("\n--- Linear Sequences ---")
  measure("Immutable List size")(immutableList.size)
  measure("Immutable List last element")(immutableList.last)
  measure("Update Immutable List (create new)")(immutableList.updated(0, 100))
  measure("Delete from Immutable List (create new)")(immutableList.filterNot(_ == 1))

  measure("Mutable ListBuffer size")(mutableList.size)
  measure("Mutable ListBuffer last element")(mutableList.last)
  measure("Update Mutable ListBuffer")(mutableList.update(0, 100))
  measure("Delete from Mutable ListBuffer")(mutableList.remove(0))

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  val array = Array(1 to 1_000_000: _*)
  val vector = (1 to 1_000_000).toVector
  val arrayBuffer = mutable.ArrayBuffer.empty[Int]
  arrayBuffer ++= (1 to 1_000_000)

  println("\n--- Indexed Sequences ---")
  measure("Vector size")(vector.size)
  measure("Vector last element")(vector.last)
  measure("Update Vector (create new)")(vector.updated(0, 100))
  measure("Delete from Vector (create new)")(vector.filterNot(_ == 1))

  measure("Array size")(array.length)
  measure("Array last element")(array.last)
  measure("Update Array")(array.updated(0, 100))
  measure("Delete from Array (create new)")(array.filterNot(_ == 1))

  measure("ArrayBuffer size")(arrayBuffer.size)
  measure("ArrayBuffer last element")(arrayBuffer.last)
  measure("Update ArrayBuffer")(arrayBuffer.update(0, 100))
  measure("Delete from ArrayBuffer")(arrayBuffer.remove(0))

  /* Sets */
  val immutableSet = (1 to 1_000_000).toSet
  val mutableSet = mutable.HashSet.empty[Int]
  mutableSet ++= (1 to 1_000_000)

  println("\n--- Sets ---")
  measure("Immutable Set size")(immutableSet.size)
  measure("Immutable Set contains 1")(immutableSet.contains(1))
  measure("Delete from Immutable Set (create new)")(immutableSet - 1)

  measure("Mutable Set size")(mutableSet.size)
  measure("Mutable Set contains 1")(mutableSet.contains(1))
  measure("Update Mutable Set (add 100)")(mutableSet.add(100))
  measure("Delete from Mutable Set")(mutableSet.remove(1))

  /* Maps */
  val immutableMap = (1 to 1_000_000).map(i => (i, i)).toMap
  val mutableMap = mutable.HashMap.empty[Int, Int]
  mutableMap ++= (1 to 1_000_000).map(i => (i, i))

  println("\n--- Maps ---")
  measure("Immutable Map size")(immutableMap.size)
  measure("Immutable Map get 1")(immutableMap.get(1))
  measure("Update Immutable Map (create new)")(immutableMap.updated(1, 100))
  measure("Delete from Immutable Map (create new)")(immutableMap - 1)

  measure("Mutable Map size")(mutableMap.size)
  measure("Mutable Map get 1")(mutableMap.get(1))
  measure("Update Mutable Map")(mutableMap.update(1, 100))
  measure("Delete from Mutable Map")(mutableMap.remove(1))

}
