package it.unibo.pps.tasks.adts


import org.junit.Assert.*
import org.junit.Test
import it.unibo.pps.u03.Sequences.Sequence.*
import it.unibo.pps.u03.Optionals.Optional.*
import it.unibo.pps.tasks.typeclasses.Ex5Traversable.*
import it.unibo.pps.tasks.typeclasses.Ex5Traversable.given


class TraversableTest:

  @Test def testTraverseSequence() =
    val seq = Cons(10, Cons(20, Cons(30, Nil())))
    var result = ""
    logAllPoly(seq)(elem => result += elem.toString + "-")
    assertEquals("10-20-30-", result)

  @Test def testTraverseSequenceNil() =
    val seq = Nil()
    var result = ""
    logAllPoly(seq)(elem => result += elem.toString + "-")
    assertEquals("", result)

  @Test def testTraverseOptionalJust() =
    val opt = Just(99)
    var result = ""
    logAllPoly(opt)(elem => result += elem.toString)
    assertEquals("99", result)

  @Test def testTraverseOptionalEmpty() =
    val emptyOpt = Empty[Int]()
    var result = ""
    logAllPoly(emptyOpt)(elem => result += "NonDovrebbeSuccedere")
    assertEquals("", result)