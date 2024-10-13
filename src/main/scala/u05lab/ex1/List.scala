package u05lab.ex1

import scala.annotation.tailrec

enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()

  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse(): List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  def take(n: Int): List[A] = this match
    case h :: t if n > 0 => h :: t.take(n - 1)
    case _ => Nil()

  /** EXERCISES */

//  def zipRight: List[(A, Int)] = {
//    def zipRightRec(list: List[A], index: Int): List[(A, Int)] = list match {
//      case h :: t => (h, index) :: zipRightRec(t, index + 1)
//      case _ => Nil()
//    }
//    zipRightRec(this, 0)
//  }

  def zipRight: List[(A, Int)] =
    foldRight((Nil(): List[(A, Int)], length - 1)) { (e, acc) =>
      val (list, idx) = acc
      ((e, idx) :: list, idx - 1)
    }._1

//  def partition(pred: A => Boolean): (List[A], List[A]) = {
//    @tailrec
//    def partitionRec(list: List[A], acc1: List[A], acc2: List[A]): (List[A], List[A]) = list match
//      case h :: t if pred(h) => partitionRec(t, h :: acc1, acc2)
//      case h :: t => partitionRec(t, acc1, h :: acc2)
//      case _ => (acc1.reverse(), acc2.reverse())
//    partitionRec(this, Nil(), Nil())
//  }

  def partition(pred: A => Boolean): (List[A], List[A]) =
    foldRight(Nil(): List[A], Nil(): List[A]) { (e, acc) =>
      if pred(e)
      then (e :: acc._1, acc._2)
      else (acc._1, e :: acc._2)
    }

//  def span(pred: A => Boolean): (List[A], List[A]) = {
//  @tailrec
//  def spanRec(list: List[A], acc1: List[A]): (List[A], List[A]) = list match
//    case h :: t if pred(h) => spanRec(t, h :: acc1)
//    case _ => (acc1.reverse(), list)
//  spanRec(this, Nil())
//  }

  def span(pred: A => Boolean): (List[A], List[A]) =
    foldRight((Nil(): List[A], Nil(): List[A])) { (e, acc) =>
      if pred(e)
      then (e :: acc._1, acc._2) 
      else (acc._1, e :: acc._2)  
    } match 
      case (prefix, rest) => (prefix, rest)
  
  /** @throws UnsupportedOperationException if the list is empty */
  def reduce(op: (A, A) => A): A = {
    if (this.isEmpty) throw new UnsupportedOperationException("Cannot reduce an empty list.")
    @tailrec
    def reduceRec(list: List[A], acc: A): A = list match
      case h :: t => reduceRec(t, op(h, acc))
      case Nil() => acc
    reduceRec(this.tail.get, this.head.get)
  }

  def takeRight(n: Int): List[A] = {
    @tailrec
    def takeRightRec(list: List[A], acc: List[A]): List[A] = list match {
      case Nil() => acc
      case h :: t => takeRightRec(t, h :: acc)
    }
    val reversedAcc = takeRightRec(this, Nil())

    reversedAcc.take(n).reverse()
  }

  def collect[B](pf: PartialFunction[A, B]): List[B] = {
    @tailrec
    def collectRec(list: List[A], out: List[B]): List[B] = list match
      case h :: t if pf.isDefinedAt(h) => collectRec(t, pf(h) :: out)
      case _ :: t => collectRec(t, out)
      case _ => out.reverse()
    collectRec(this, Nil())
  }


