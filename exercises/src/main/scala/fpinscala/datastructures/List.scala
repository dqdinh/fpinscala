package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }


  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /*
  (3.1) 3
  */

  /*
  (3.2)
    Implement the function tail for removing the first element of a List. Note that the
    function takes constant time. What are different choices you could make in your
    implementation if the List is Nil? We’ll return to this question in the next chapter.
  */
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  }

  /*
  (3.3)
    Using the same idea, implement the function setHead for replacing the first element
    of a List with a different value.
  */
  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(h, xs)
    }
  }

  /*
  (3.4)
    Generalize tail to the function drop, which removes the first n elements from a list.
    Note that this function takes time proportional only to the number of elements being
    dropped—we don’t need to make a copy of the entire List.
  */
  def drop[A](l: List[A], n: Int): List[A] = {
    n match {
      case neg if neg < 0 => Nil
      case 0 => l
      case _ => drop(tail(l), n-1)
    }
  }

  /*
  (3.5)
    Implement dropWhile, which removes elements from the List prefix as long as they
    match a predicate.
  */
  def head[A](l: List[A]): A = {
    l match {
      case Cons(x, xs) => x
    }
  }

  // NOTE: resulting List is reversed
  // Q: ugly and result is reversed ... is there a way to write this using PM?
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) =>
        if (f(x)) dropWhile(xs, f)
        else Cons(x, dropWhile(xs, f))
    }
  }


  /*
  (3.6)
    Not everything works out so nicely. Implement a function, init, that returns a List
    consisting of all but the last element of a List. So, given List(1,2,3,4), init will
    return List(1,2,3). Why can’t this function be implemented in constant time like
    tail?
  */
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) =>
        if (xs == Nil) Nil
        else Cons(x, init(xs))
    }
  }

  /*
  (3.7)
    Can product, implemented using foldRight, immediately halt the recursion and
    return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting
    might work if you call foldRight with a large list. This is a deeper question that we’ll
    return to in chapter 5.

    No because f() is not executed until all inner foldRight() in the recusion steps finish. In other
    words, f() is the last thing that happends on 'case Cons(x, xs) => f(x, foldRight(xs, z)(f))' so
    you end up recursively going through foldRight before f() which prevents short-ciruiting on f()
  */

  /*
  (3.8)
    See what happens when you pass Nil and Cons themselves to foldRight, like this:
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)). What do you think this says
    about the relationship between foldRight and the data constructors of List?

    Gives you the indenity of List(1,2,3). Their mechanism of appending Nil at the end and
    then running Cons(x, xs) is similar.
  */

  /*
  (3.9)
  */
  def lengthFR[A](l: List[A]): Int = {
    foldRight(l,0)((x,y) => (1 + y))
  }

  /*
  (3.10)
    Our implementation of foldRight is not tail-recursive and will result in a StackOver- flowError for large lists (we say it’s not stack-safe). Convince yourself that this is the case, and then write another general list-recursion function, foldLeft, that is tail-recursive, using the techniques we discussed in the previous chapter. Here is its signature

  For reference:

    def foldRight[A,B](as: List[A], acc: B)(f: (A, B) => B): B = // Utility functions
      as match {
        case Nil => acc
        case Cons(h, t) => f(h, foldRight(t, acc)(f))
      }

  */

  // Q: (f: (B, A)) order of args is arbitary right?
  def foldLeft[A,B](l: List[A], acc: B)(f: (B, A) => B): B = {
    l match {
      case Nil => acc
      case Cons(h,t) => foldLeft(t, f(acc,h))(f)
    }
  }

  /*
  (3.11)
    Implement sum, product, and length using foldLeft
  */
  def sumFL(l: List[Int]) =
    foldLeft(l, 0)((x,y) => (y + x))

  def productFL(l: List[Int]) =
    foldLeft(l, 1)((x,y) => (y * x))

  def lengthFL[A](l: List[A]) =
    foldLeft(l, 0)((acc,h) => (acc + 1))

  /*
  (3.12)
    Implement reverse
  */
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc,h) => (Cons(h,acc)))
  /*
  (3.14)
    Append 2 lists
  */

  def append[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((x,y) => Cons(x,y))

  /*
  (3.16)
  NOTE: similar to Flatten
  */
  def concat[A](lofl: List[List[A]]): List[A] =
    foldRight(lofl, List[A]())((x,y) => append(x,y))

  /*
  (3.16)
  */

  def add1(l: List[Int]): List[Int] =
    foldLeft(l, List[Int]())((acc, h) => Cons((h+1), acc))

  /*
  (3.17)
  */
  def listToString(l: List[Double]): List[String] =
    foldLeft(l, List[String]()) ((acc, h) => Cons(h.toString, acc))

  /*
  (3.18)
  */
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldLeft(l, List[B]())((acc, h) => Cons(f(h), acc))

  /*
  (3.19)
  */
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldLeft(l, List[A]())(
      (acc, h) =>
        if (f(h)) Cons(h, acc)
        else acc
    )

  /*
  (3.20)
  */
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldLeft(l, List[B]())((acc, h) => concat(List(acc, f(h))))

  /*
  (3.21)
  */
  def filterFM[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(
      i =>
        if (f(i)) List(i)
        else List[A]()
    )

  /*
  (3.22)
  */
  def listSum(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1,l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1 + h2, listSum(t1,t2))
    }
  }

  /*
  (3.23)
  */
  def zipWith[A,B](l1: List[A], l2: List[A])(f: (A,A) => B): List[B] = {
    (l1,l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }
  }

  /*
  (3.24)
  Wikipedia Definition: a subsequence is a sequence that can be derived from another sequence by deleting some elements without changing the order of the remaining elements. For example, the sequence <A,B,D>  is a subsequence of <A,B,C,D,E,F> . They should not be confused with substring which is <A,B,C,D>  for above string and which is a refinement of subsequence.

  Takeaway: has to be ordered

  Strategy 1: use sup as the source of truth. Go through sub (like foldLeft's mech to leverage short cuircitihg). A you go through sub, redefine the sup sequence after you've found it from sub. that way you can track order.
  */

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
  }

}






































