package it.unibo.pps.tasks.typeclasses

import it.unibo.pps.u03.Sequences.Sequence, Sequence.*
import it.unibo.pps.u03.Optionals.*

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[T[_]]:
    def traverse[A](ta: T[A])(consumer: A => Unit): Unit

  given Traversable[Optional] with
    override def traverse[A](ta: Optional[A])(consumer: A => Unit): Unit = ta match
      case Optional.Just(a) => consumer(a)
      case Optional.Empty() => ()

  given Traversable[Sequence] with
    override def traverse[A](ta: Sequence[A])(consumer: A => Unit): Unit = ta match
      case Cons(h,t) =>
        consumer(h)
        traverse(t)(consumer)
      case Nil() => ()


  def log[A](a: A): Unit = println("The next element is: "+a)

  def logAll[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAll(t)
    case _ => ()

  def logAllPoly[T[_]: Traversable, A](container: T[A])(consumer: A => Unit): Unit =
    val summable = summon[Traversable[T]]
    summable.traverse(container)(consumer)

  
