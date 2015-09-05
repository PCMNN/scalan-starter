package scalan.examples

import scalan._
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}

trait MyArrays { self: ExampleDsl =>

  /**
   * Optional type synonim to hide boilerplate of using Rep
   */
  type MyArr[T] = Rep[MyArray[T]]

  /**
   * User defined type
   */
  trait MyArray[T] extends Reifiable[MyArray[T]] {
    implicit def eT: Elem[T]
    def length: Rep[Int]
    def values: Rep[Collection[T]]
    def apply(i: Rep[Int]): Rep[T]
    def map[B: Elem](f: Rep[T] => Rep[B]): MyArr[B] = MyArray(values.map(f))
    def mapBy[B: Elem](f: Rep[T => B]): MyArr[B] = MyArray(values.mapBy(f))
    def zip[B: Elem](ys: MyArr[B]): MyArr[(T, B)] = PairMyArray((values zip ys.values).convertTo[Collection[(T, B)]])
  }

  /**
   * Declare this implicit to specify how to build Collection descriptor (Elem) generically using 
   * descriptor of its element type.
   * This is a generic function that builds descriptors from descriptors
   * @tparam A type of array element
   * @return default descriptor for a given element type
   */
  /*implicit def defaultMyArrayElement[A:Elem]: Elem[MyArray[A]] = element[A] match {
    case _: BaseElem[_] => element[BaseMyArray[A]].asElem[MyArray[A]]
    case pe: PairElem[a, b] =>
      implicit val ea = pe.eFst
      implicit val eb = pe.eSnd
      element[PairMyArray[a, b]].asElem[MyArray[A]]
    case viewE: ViewElem[_, _] => element[BaseMyArray[A]].asElem[MyArray[A]]
    case e => ???(s"Element is $e")
  }*/

  /**
   * Companion for MyArray type. Naming convention is used here.
   * Generated companion MyArray is inherited from this trait.
   */
  trait MyArrayCompanion extends TypeFamily1[MyArray] {
    /**
     * Constructs MyArray from Collection and can be invoked using companion like this
     * val my = MyArray(arr)
     * @param arr
     * @tparam T
     * @return
     */
    def apply[T: Elem](coll: Rep[Collection[T]]): MyArr[T] = fromArray(coll)

    /**
     * Construct MyArray in a generic way using type descriptor of its elements.
     * This funtions takes input array and puts it into different structures depending on element type.
     * @param arr input array
     * @tparam T type of array elements
     * @return
     */
    def fromArray[T: Elem](arr: Rep[Collection[T]]): MyArr[T] = {
      element[T] match {
        case baseE: BaseElem[a] =>
          BaseMyArray[a](arr.asRep[Collection[a]])//.asRep[MyArray[a]]
        case pairE: PairElem[a, b] =>
          implicit val ea = pairE.eFst
          implicit val eb = pairE.eSnd
          val ps = arr.asRep[Collection[(a, b)]]
          val as = fromArray(ps.map { _._1 })
          val bs = fromArray(ps.map { _._2 })
          //val res = as zip bs
          //res.convertTo[MyArray[(a, b)]] //
          PairMyArray[a,b](as, bs)
        case viewE: ViewElem[a, b] =>
          BaseMyArray[b](arr.asRep[Collection[b]])
        case e => ???(s"Element is $e")
      }
    }

    /**
     * Another example of MyArray constructor. Uses core Collection primitives.
     * @param len number of elements in the new MyArray
     * @param v   value to put into each element of MyArray
     * @tparam T
     * @return
     */
    def replicate[T: Elem](len: Rep[Int], v: Rep[T]): MyArr[T] = {
      element[T] match {
        case baseE: BaseElem[a] =>
          BaseMyArray[a](Collection.replicate(len, v.asRep[a]))
        case pairElem: PairElem[a ,b] => {
          implicit val ea = pairElem.eFst
          implicit val eb = pairElem.eSnd
          val ps = v.asRep[(a, b)]
          val as = replicate(len, ps._1)
          val bs = replicate(len, ps._2)
          as zip bs
        }
        case viewElem: ViewElem[a, b] =>
          BaseMyArray(Collection.replicate(len, v))
        case e => ???(s"Element is $e")
      }
    }
  }

  abstract class UnitMyArray(val length: Rep[Int]) extends MyArray[Unit] {
    def eT = UnitElement
    def values = Collection.replicate(length, ())
    def apply(i: Rep[Int]) = ()
  }
  abstract class BaseMyArray[T](val values: Rep[Collection[T]])(implicit val eT: Elem[T]) extends MyArray[T] {
    def length = values.length
    def apply(i: Rep[Int]) = values(i)
  }
  abstract class PairMyArray[A, B](val values: Rep[Collection[(A, B)]])(implicit val eA: Elem[A], val eB: Elem[B])
    extends MyArray[(A, B)] {
    lazy val eT = element[(A, B)]
    def arr = as.arr zip bs.arr
    def as: Rep[Collection[A]] = values.convertTo[PairCollection[A, B]].as
    def bs: Rep[Collection[B]] = values.convertTo[PairCollection[A, B]].bs
    def apply(i: Rep[Int]) = values(i)
    def length = as.length
  }

  trait UnitMyArrayCompanion extends ConcreteClass0[UnitMyArray] {
    //def apply(coll: Rep[Collection[Unit]]): MyArr[Unit] = UnitMyArray(coll.asRep[Collection[Unit]]).asRep[MyArray[Unit]]
  }
  trait BaseMyArrayCompanion extends ConcreteClass1[BaseMyArray] {
    //def apply[T: Elem](coll: Rep[Collection[T]]): MyArr[T] = BaseMyArray[T](coll.asRep[Collection[T]]).asRep[MyArray[T]]
  }
  trait PairMyArrayCompanion extends ConcreteClass2[PairMyArray] with MyArrayCompanion {
    def apply[A, B](as: Rep[MyArray[A]], bs: Rep[MyArray[B]])(implicit ea: Elem[A], eb: Elem[B]): MyArray[(A, B)] = {
      PairMyArray((as.values zip bs.values).convertTo[Collection[(A, B)]]).convertTo[MyArray[(A, B)]]
    }
  }
}

trait MyArraysDsl extends impl.MyArraysAbs with MyArrays { self: ExampleDsl => }

trait MyArraysDslSeq extends MyArraysDsl with impl.MyArraysSeq with ScalanSeq { self: ExampleDslSeq => }

trait MyArraysDslExp extends MyArraysDsl with impl.MyArraysExp with ScalanExp { self: ExampleDslExp => }
