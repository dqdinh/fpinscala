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

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
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
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
      as match {
        case Nil => z
        case Cons(h, t) => f(h, foldRight(t, z)(f))
      }
  */
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }
  }

  /*
  (3.11)
    Implement sum, product, and length using foldLeft
  */
  def sumLF(l: List[Int]) =
    foldLeft(l, 0)((x,y) => (y + x))

  def productLF(l: List[Int]) =
    foldLeft(l, 1)((x,y) => (y * x))

  def lengthLF(l: List[Int]) =
    foldLeft(l, 0)((x,y) => (y + 1))

  /*
  (3.12)
    Implement reverse
  */
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc,h) => (Cons(h,acc)))


  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}
