package conversion
import parser.Token.Type.False

import scala.collection.mutable

/**
 * This linked hashset provides an ordered hashset
 *
 * @tparam A The element type.
 */
class LinkedSet[A] extends mutable.Iterable[A]{
  class Node(val value: Option[A], var next: Node = null, var prev: Node = null)

  private val nodeMap = mutable.HashMap.empty[A, Node]
  private val head_ : Node = Node(None)
  private val tail_ : Node = Node(None)
  private var size_ : Int = 0

  head_.next = tail_
  tail_.prev = head_

  def length: Int = size_

  override def head: A =
    if size_ == 0 then throw java.util.NoSuchElementException("Empty linked set has no head")
    else head_.next.value match
      case None => throw IllegalStateException("Internal error: null value inside element node!")
      case Some(v: A) => v

  override def last: A =
    if size_ == 0 then throw java.util.NoSuchElementException("Empty linked set has no head")
    else tail_.prev.value match
      case None => throw IllegalStateException("Internal error: null value inside element node!")
      case Some(v: A) => v

  def this(coll: IterableOnce[A]) =
    this()
    coll.iterator.foreach(add)

  private def insertBefore(cur: Node, toInsert: Node): Unit =
    val prev = cur.prev
    prev.next = toInsert
    toInsert.prev = prev
    toInsert.next = cur
    cur.prev = toInsert

  /**
   * Prepend the element, if it is not present.
   * Time complexity: O(1)
   *
   * @param element The element to append at the end
   * @return true if element exists in the hashset, false otherwise
   */
  def prepend(element: A): Boolean =
    if nodeMap.contains(element) then return false
    size_ += 1
    val newHead = Node(Some(element))
    nodeMap.put(element, newHead)
    insertBefore(head_.next, newHead)
    true

  /**
   * Append the element, if it is not present.
   * Time complexity: O(1)
   *
   * @param element The element to append at the end
   * @return true if element exists in the hashset, false otherwise
   */
  def add(element: A): Boolean =
    if nodeMap.contains(element) then return false
    size_ += 1
    val newTail = Node(Some(element))
    nodeMap.put(element, newTail)
    insertBefore(tail_, newTail)
    true

  /**
   *
   * @param coll collection to be added
   */
  def addAll(coll: IterableOnce[A]): Unit = coll.iterator.foreach(add)

  /**
   * Insert an element before another element, if the element to insert does not exist and the other element exists.
   * Time complexity: O(1)
   *
   * @param cur      The element which the new element will be inserted before
   * @param toInsert The element to insert
   * @return true if insertion succeeded, false otherwise
   */
  def insertBefore(cur: A, toInsert: A): Boolean =
    nodeMap.get(cur) match
      case None => false
      case Some(node: Node) =>
        if nodeMap.contains(toInsert) then return false
        val newNode = Node(Some(toInsert))
        nodeMap.put(toInsert, newNode)
        insertBefore(node, newNode)
        size_ += 1
        true


  /**
   * Insert an element after another element, if the element to insert does not exist and the other element exists.
   * Time complexity: O(1)
   *
   * @param cur      The element which the new element will be inserted before
   * @param toInsert The element to insert
   * @return true if insertion succeeded, false otherwise
   */
  def insertAfter(cur: A, toInsert: A): Boolean =
    nodeMap.get(cur) match
      case None => false
      case Some(node: Node) =>
        if nodeMap.contains(toInsert) then return false
        val newNode = Node(Some(toInsert))
        nodeMap.put(toInsert, newNode)
        insertBefore(node.next, newNode)
        size_ += 1
        true


  private def removeNode(node: Node): Unit =
    val prev = node.prev
    val next = node.next
    prev.next = next
    next.prev = prev
    node.prev = null
    node.next = null


  /**
   * Remove the element, if it is present.
   * Time complexity: O(1)
   *
   * @param element The element to remove
   * @return true if element exists in the hashset, false otherwise
   */
  def remove(element: A): Boolean =
    nodeMap.get(element) match
      case None => false
      case Some(node: Node) =>
        removeNode(node)
        nodeMap.remove(element)
        size_ -= 1
        true

  private def removeHead(): A =
    if size_ == 0 then throw java.util.NoSuchElementException()
    head_.next.value match
      case None => throw IllegalStateException("Internal error: null value inside element node!")
      case Some(v: A) =>
        removeNode(head_.next)
        size_ -= 1
        v

  def contains(element: A): Boolean = nodeMap.contains(element)

  override def iterator: Iterator[A] = toList.iterator

  override def toList: List[A] =
    val list = mutable.ListBuffer.empty[A]
    var cur = head_.next
    while(cur != tail_){
      cur.value match
        case None => throw IllegalStateException("Internal error: null value inside element node!")
        case Some(v: A) => list.addOne(v)
      cur = cur.next
    }
    list.toList

  override def map[B](f: A => B): LinkedSet[B] =
    val newSet = LinkedSet[B]
    this.foreach(e => newSet.add(f(e)))
    newSet

  override def toString: String =
    var s = "LinkedSet("
    var cur = head_.next
    for (i <- 0 until size)
      if i > 0 then s += ", "
      cur.value match
        case None => throw IllegalStateException("Internal error: null value inside element node!")
        case Some(v: A)  => s += v.toString
      cur = cur.next
    s += ")"
    s

  def apply(n: Int): A =
    if n >= size || n < 0 then throw IndexOutOfBoundsException(s"The set has $size_ elements, index $n is out of bound")
    else
      if n > size / 2 then
        var cur = tail_.prev
        for (_ <- n until size-1)
          cur = cur.prev
        cur.value match
          case None => throw IllegalStateException("Internal error: null value inside element node!")
          case Some(v: A) => v

      else
        var cur = head_.next
        for (_ <- 0 until n)
          cur = cur.next
        cur.value match
          case None => throw IllegalStateException("Internal error: null value inside element node!")
          case Some(v: A) => v
}
