package org.primetalk.advent3.tools

// import cats.collections.{Heap => CatHeap}
// import cats._, cats.implicits._, cats.collections._, cats.collections.syntax.all._
import cats.collections.Heap
import cats.kernel.Order

trait MyHeap[H[*]]:
  def empty[T]: H[T]
  def enqueue[T: Ordering](h: H[T], el: T): H[T]
  def dequeue[T: Ordering](h: H[T]): (T, H[T])
  def isEmpty[T: Ordering](h: H[T]): Boolean

given MyHeap[Heap] with
  def empty[T]: Heap[T] = 
    Heap.empty
  def enqueue[T: Ordering](h: Heap[T], el: T): Heap[T] = 
    given Order[T] = Order.fromOrdering
    h.add(el)

  def dequeue[T: Ordering](h: Heap[T]): (T, Heap[T]) = 
    given Order[T] = Order.fromOrdering
    val el = h.getMin
    (el.getOrElse(throw IllegalArgumentException("dequeue on empty heap")), h.remove)

  def isEmpty[T: Ordering](h: Heap[T]): Boolean = 
    h.isEmpty
