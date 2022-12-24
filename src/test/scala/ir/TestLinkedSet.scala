package ir
import org.scalatest.flatspec.AnyFlatSpec
import ir.LinkedSet

class TestLinkedSet extends AnyFlatSpec {
  "A linked set" should "add elements correctly" in {
    val l = List(1,2,3,4,5)
    val set = LinkedSet[Int](l)
    assert(l == set.toList)
  }

  it should "remove elements correctly" in {
    val l = List(1,2,3,4,5,6)
    val set = LinkedSet[Int](l)

    set.remove(6)
    assert(List(1,2,3,4,5) == set.toList)

    set.remove(1)
    assert(List(2,3,4,5) == set.toList)

    set.remove(3)
    assert(List(2,4,5) == set.toList)

    set.remove(4)
    assert(List(2,5) == set.toList)

    set.remove(5)
    assert(List(2) == set.toList)

    set.remove(2)
    assert(List() == set.toList)
  }

  it should "give head correctly" in {
    val set = LinkedSet[Int]()
    assertThrows[NoSuchElementException](set.head)

    set.add(1)
    assert(set.head == 1)

    set.add(2)
    assert(set.head == 1)
  }

  it should "give last correctly" in {
    val set = LinkedSet[Int]()
    assertThrows[NoSuchElementException](set.last)

    set.add(1)
    assert(set.last == 1)

    set.add(2)
    assert(set.last == 2)
  }

  it should "insert elements correctly" in {
    val l = List(1)
    val set = LinkedSet[Int](l)

    set.insertBefore(1, 0)
    assert(List(0,1) == set.toList)

    set.insertBefore(1, 2)
    assert(List(0,2,1) == set.toList)

    assert(! set.insertBefore(3, 4))
    assert(! set.insertBefore(1, 2))

    set.insertAfter(0, 3)
    assert(List(0,3,2,1) == set.toList)

    set.insertAfter(1, 4)
    assert(List(0, 3, 2, 1,4) == set.toList)

    assert(! set.insertAfter(5, 6))
    assert(! set.insertAfter(1, 2))
  }

  it should "be applied correctly" in {
    val l = List(1,2,3,4,5,6,7)
    val set = LinkedSet[Int](l)
    assertThrows[IndexOutOfBoundsException](
      set(-1)
    )
    assertThrows[IndexOutOfBoundsException](
      set(7)
    )

    assert(1 == set(0))
    assert(2 == set(1))
    assert(3 == set(2))
    assert(4 == set(3))
    assert(5 == set(4))
    assert(6 == set(5))
    assert(7 == set(6))
  }
}
