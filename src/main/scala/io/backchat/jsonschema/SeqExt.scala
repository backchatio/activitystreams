package io.backchat.jsonschema

import scalaz.Reducer
import scalaz.Generator._
import collection.{GenSeq, GenIterable}

object SeqExt {
  /**
   * extrator for the first element of Seq[T]
   */
  object +: {
    def unapply[T](l: Seq[T]): Option[(T, Seq[T])] = {
      if(l.isEmpty) None
      else          Some(l.head, l.tail)
    }
  }

  /**
   * extrator for the last element of Seq[T]
   */
  object :+ {
    def unapply[T](l: Seq[T]): Option[(Seq[T], T)] = {
      if(l.isEmpty) None
      else          Some(l.init, l.last)
    }
  }

  /** @return an extension for a seq */
  implicit def extendSeq[T](seq: Seq[T]): ExtendedSeq[T] = new ExtendedSeq(seq)
  /**
   * Additional methods for seqs
   */
  class ExtendedSeq[T](seq: Seq[T]) {

    def reduceWith[S](reducer: Reducer[T, S]) = FoldlGenerator[Seq].reduce(reducer, seq)

    def updateLast(f: T => T) = seq match {
      case s :+ last => s :+ f(last)
      case other     => other
    }

    /**
     * remove the first element satisfying the predicate
     * @return a seq minus the first element satisfying the predicate
     */
    def removeFirst(predicate: T => Boolean): Seq[T] = {
      val (withoutElement, startWithElement) = seq span (x => !predicate(x))
      withoutElement ++ startWithElement.drop(1)
    }

    /**
     * @return all the elements in seq which are not in other, even if they are duplicates: Seq(1, 1).diff(Seq(1)) == Seq(1)
     *         this uses a user given comparison function
     */
    def delta[S](other: Seq[S], compare: (T, S) => Boolean): Seq[T] = {
      def notFound(ls1: Seq[T], ls2: Seq[S], result: Seq[T] = Seq()): Seq[T] =
        ls1 match {
          case Seq()        => result
          case head +: rest =>
            if  (ls2.exists(compare(head, _))) notFound(rest, ls2.removeFirst(l => compare(head, l)), result)
            else                               notFound(rest, ls2, result :+ head)
        }
      notFound(seq, other)
    }
  }

  /**
   * implicit definition to transform an Iterable to an ExtendedIterable
   */
  implicit def extendIterable[T](xs : GenIterable[T]): ExtendedIterable[T] = new ExtendedIterable(xs)

  /**
   * Additional methods for Iterable objects
   */
  class ExtendedIterable[T](xs: GenIterable[T]) {

    /**
     * @return true if the 2 iterables contain the same elements, in the same order,
     *         according to a function f
     */
    def isSimilar[S >: T](that: GenIterable[S], f: Function2[T, S, Boolean]): Boolean = {
      val it1 = xs.iterator
      val it2 = that.iterator
      var res = true
      while (res && it1.hasNext && it2.hasNext) {
        res = f(it1.next, it2.next)
      }
      !it1.hasNext && !it2.hasNext && res
    }
    /**
     * @return true if the 2 iterables contain the same elements recursively, in any order
     */
    def sameElementsAs(that: GenIterable[T]): Boolean = sameElementsAs(that, (x, y) => x == y)
    /**
     * @return true if the 2 iterables contain the same elements (according to a comparison function f) recursively, in any order
     */
    def sameElementsAs(that: GenIterable[T], f: (T, T) => Boolean): Boolean = {
      def isNotItsOwnIterable(a: GenIterable[_]) = a.isEmpty || a.iterator.next != a
      def matchTwo(x: T, y: T): Boolean = {
        (x, y) match {
          case (a: GenIterable[_], b: GenIterable[_]) if (isNotItsOwnIterable(a)) =>
            x.asInstanceOf[GenIterable[T]].sameElementsAs(y.asInstanceOf[GenIterable[T]], f)
          case _ => f(x, y)
        }
      }
      val ita = xs.iterator.toList
      val itb = that.iterator.toList
      (ita, itb) match {
        case (Nil, Nil) => true
        case (a: GenIterable[_], b: GenIterable[_]) => {
           (a.headOption.isDefined && b.headOption.isDefined) && {
            val (x, y, resta, restb) = (a.head, b.head, a.drop(1), b.drop(1))
            matchTwo(x, y) && resta.sameElementsAs(restb, f) ||
            resta.exists(matchTwo(_, y)) && restb.exists(matchTwo(x, _)) &&
              resta.removeFirst(matchTwo(_, y)).sameElementsAs(restb.removeFirst(matchTwo(x, _)), f)
          }
        }
        case _ => ita == itb
      }
    }
    /**
     * @return true if the second iterable elements are contained in the first, in order
     */
    def containsInOrder(l: T*): Boolean = {
      val firstList = xs.toList
      val secondList = l.toList
      (firstList, secondList) match {
         case (_, Nil) => true
         case (Nil, _) => false
         case (a :: Nil, b :: Nil) => a == b
         case (a :: firstRest, b :: secondRest) => {
           if (a != b)
             firstRest.containsInOrder(secondList:_*)
           else
             firstRest.containsInOrder(secondRest:_*)
         }
      }
    }
    /**
     * @return the representation of the elements of the iterable using the toString method recursively
     */
    def toDeepString: String = {
      if (!xs.isEmpty && xs == xs.iterator.next)
        xs.toString
      else
        "[" + xs.toList.map { x =>
          x match {
            case i: GenIterable[_] => i.toDeepString
            case _ => x.toString
          }
       }.mkString(", ") + "]"
    }
    /** map the first element with a function */
    def mapFirst(f: T => T): GenSeq[T] = (xs.take(1).map(f) ++ xs.drop(1)).toSeq
    /** map the last element with a function */
    def mapLast(f: T => T): Seq[T] = (xs.seq.dropRight(1) ++ xs.seq.takeRight(1).map(f)).toSeq
    /** reduce a list from left to right */
    def reduceWith[S](reducer: Reducer[T, S]) = FoldlGenerator[Iterable].reduce(reducer, xs.seq)
    /** @return a sequence rotated of a number of elements */
    def rotate(n: Int) = xs.slice(n, xs.size) ++ xs.slice(0, n)
    /** @return a randomly mixed sequence */
    def scramble = {
      val random = new java.util.Random
      // rotate arbitrarily the sequence first then sort randomly
      xs.rotate(random.nextInt(xs.size+1)).seq.toSeq.sortWith((_,_) => random.nextInt(2) > 0)
    }

  }
}
