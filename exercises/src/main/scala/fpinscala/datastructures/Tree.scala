package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Branch(Branch(Leaf(21), Leaf(33)), Leaf(33))

  /*
  (3.25)
  */
  def size[A](t: Tree[A]): Int = {
    def go(t: Tree[A], acc: Int): Int = {
      t match {
        case Leaf(v) => acc + 1
        case Branch(l, r) => go(l, acc) + go(r, acc)
      }
    }
    go(t, 0)
  }

  /*
  (3.25)
  */
  def max(t: Tree[Int]) = {
    def go(t: Tree[Int], acc: Int): Int = {
      t match {
        case Leaf(v) => acc.max(v)
        case Branch(l, r) => go(l, go(r, acc))
      }
    }
    go(t, 0)
  }

  /*
  (3.25)
  */
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  /*
  (3.29)
  */
  def size[A](t: Tree[A]): Int = {
    fold(t)(v => 1)((l,r) => (l + r + 1))
  }

  def max(t: Tree[Int]): Int = {
    fold(t)(v => v)((l,r) => l.max(r))
  }

  def depth[A](t: Tree[A]): Int = {
    fold(t)(v => 0)((l,r) => (l+1).max(r+1))
  }

  def map[A,B](t: Tree[A])(f: A => B) = {
    fold(t)(v => Leaf(f(v)))((l,r) => Branch(l,r))
  }

}



































