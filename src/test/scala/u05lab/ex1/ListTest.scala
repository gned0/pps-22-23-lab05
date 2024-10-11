package u05lab.ex1

import org.junit.Assert.{assertEquals, assertThrows, fail}
import org.junit.Test

class ListTest {

  // Helper to create List easily
  def listOf[A](elements: A*): List[A] = {
    elements.foldRight(List.Nil(): List[A])((e, acc) => e :: acc)
  }

  @Test
  def testZipRight(): Unit = {
    val list = listOf("a", "b", "c")
    val zipped = list.zipRight
    assertEquals(listOf(("a", 0), ("b", 1), ("c", 2)), zipped)
  }

  @Test
  def testPartition(): Unit = {
    val list = listOf(1, 2, 3, 4)
    val (evens, odds) = list.partition(_ % 2 == 0)
    assertEquals(listOf(2, 4), evens)
    assertEquals(listOf(1, 3), odds)
  }

  @Test
  def testSpan(): Unit = {
    val list = listOf(1, 2, 3, 4, 5)
    val (prefix, suffix) = list.span(_ < 4)
    assertEquals(listOf(1, 2, 3), prefix)
    assertEquals(listOf(4, 5), suffix)
  }

  @Test
  def testReduce(): Unit = {
    val list = listOf(1, 2, 3, 4)
    val reduced = list.reduce(_ + _)
    assertEquals(10, reduced)

    try {
      List.Nil[Int]().reduce(_ + _)
      fail("Expected UnsupportedOperationException")
    } catch {
      case _: UnsupportedOperationException => // expected
    }
  }

  @Test
  def testTakeRight(): Unit = {
    val list = listOf(1, 2, 3, 4, 5)
    val taken = list.takeRight(3)
    assertEquals(listOf(3, 4, 5), taken)
  }

  @Test
  def testCollect(): Unit = {
    val list = listOf(1, 2, 3, 4, 5)
    val collected = list.collect { case x if x % 2 == 0 => x * 10 }
    assertEquals(listOf(20, 40), collected)
  }
}
