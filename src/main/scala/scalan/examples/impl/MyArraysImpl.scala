package scalan.examples

import scalan._
import scalan.common.Default
import scalan.common.OverloadHack.Overloaded1
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait MyArraysAbs extends MyArrays with scalan.Scalan {
  self: ExampleDsl =>

  // single proxy for each type family
  implicit def proxyMyArray[T](p: Rep[MyArray[T]]): MyArray[T] = {
    proxyOps[MyArray[T]](p)(scala.reflect.classTag[MyArray[T]])
  }

  // familyElem
  class MyArrayElem[T, To <: MyArray[T]](implicit val eT: Elem[T])
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val entityDef: STraitOrClassDef = {
      val module = getModules("MyArrays")
      module.entities.find(_.name == "MyArray").get
    }
    lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[MyArray[T]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Reifiable[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[MyArray[T]] => convertMyArray(x) }
      tryConvert(element[MyArray[T]], this, x, conv)
    }

    def convertMyArray(x : Rep[MyArray[T]]): Rep[To] = {
      assert(x.selfType1 match { case _: MyArrayElem[_, _] => true; case _ => false })
      x.asRep[To]
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit def myArrayElement[T](implicit eT: Elem[T]): Elem[MyArray[T]] =
    new MyArrayElem[T, MyArray[T]]

  implicit case object MyArrayCompanionElem extends CompanionElem[MyArrayCompanionAbs] {
    lazy val tag = weakTypeTag[MyArrayCompanionAbs]
    protected def getDefaultRep = MyArray
  }

  abstract class MyArrayCompanionAbs extends CompanionBase[MyArrayCompanionAbs] with MyArrayCompanion {
    override def toString = "MyArray"
  }
  def MyArray: Rep[MyArrayCompanionAbs]
  implicit def proxyMyArrayCompanion(p: Rep[MyArrayCompanion]): MyArrayCompanion =
    proxyOps[MyArrayCompanion](p)

  // elem for concrete class
  class UnitMyArrayElem(val iso: Iso[UnitMyArrayData, UnitMyArray])
    extends MyArrayElem[Unit, UnitMyArray]
    with ConcreteElem[UnitMyArrayData, UnitMyArray] {
    override lazy val parent: Option[Elem[_]] = Some(myArrayElement(UnitElement))
    override lazy val entityDef = {
      val module = getModules("MyArrays")
      module.concreteSClasses.find(_.name == "UnitMyArray").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map()
    }

    override def convertMyArray(x: Rep[MyArray[Unit]]) = UnitMyArray(x.length)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      weakTypeTag[UnitMyArray]
    }
  }

  // state representation type
  type UnitMyArrayData = Int

  // 3) Iso for concrete class
  class UnitMyArrayIso
    extends Iso[UnitMyArrayData, UnitMyArray] {
    override def from(p: Rep[UnitMyArray]) =
      p.length
    override def to(p: Rep[Int]) = {
      val length = p
      UnitMyArray(length)
    }
    lazy val defaultRepTo: Rep[UnitMyArray] = UnitMyArray(0)
    lazy val eTo = new UnitMyArrayElem(this)
  }
  // 4) constructor and deconstructor
  abstract class UnitMyArrayCompanionAbs extends CompanionBase[UnitMyArrayCompanionAbs] with UnitMyArrayCompanion {
    override def toString = "UnitMyArray"

    def apply(length: Rep[Int]): Rep[UnitMyArray] =
      mkUnitMyArray(length)
  }
  object UnitMyArrayMatcher {
    def unapply(p: Rep[MyArray[Unit]]) = unmkUnitMyArray(p)
  }
  def UnitMyArray: Rep[UnitMyArrayCompanionAbs]
  implicit def proxyUnitMyArrayCompanion(p: Rep[UnitMyArrayCompanionAbs]): UnitMyArrayCompanionAbs = {
    proxyOps[UnitMyArrayCompanionAbs](p)
  }

  implicit case object UnitMyArrayCompanionElem extends CompanionElem[UnitMyArrayCompanionAbs] {
    lazy val tag = weakTypeTag[UnitMyArrayCompanionAbs]
    protected def getDefaultRep = UnitMyArray
  }

  implicit def proxyUnitMyArray(p: Rep[UnitMyArray]): UnitMyArray =
    proxyOps[UnitMyArray](p)

  implicit class ExtendedUnitMyArray(p: Rep[UnitMyArray]) {
    def toData: Rep[UnitMyArrayData] = isoUnitMyArray.from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoUnitMyArray: Iso[UnitMyArrayData, UnitMyArray] =
    new UnitMyArrayIso

  // 6) smart constructor and deconstructor
  def mkUnitMyArray(length: Rep[Int]): Rep[UnitMyArray]
  def unmkUnitMyArray(p: Rep[MyArray[Unit]]): Option[(Rep[Int])]

  // elem for concrete class
  class BaseMyArrayElem[T](val iso: Iso[BaseMyArrayData[T], BaseMyArray[T]])(implicit eT: Elem[T])
    extends MyArrayElem[T, BaseMyArray[T]]
    with ConcreteElem[BaseMyArrayData[T], BaseMyArray[T]] {
    override lazy val parent: Option[Elem[_]] = Some(myArrayElement(element[T]))
    override lazy val entityDef = {
      val module = getModules("MyArrays")
      module.concreteSClasses.find(_.name == "BaseMyArray").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("T" -> Left(eT))
    }

    override def convertMyArray(x: Rep[MyArray[T]]) = BaseMyArray(x.values)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[BaseMyArray[T]]
    }
  }

  // state representation type
  type BaseMyArrayData[T] = Collection[T]

  // 3) Iso for concrete class
  class BaseMyArrayIso[T](implicit eT: Elem[T])
    extends Iso[BaseMyArrayData[T], BaseMyArray[T]] {
    override def from(p: Rep[BaseMyArray[T]]) =
      p.values
    override def to(p: Rep[Collection[T]]) = {
      val values = p
      BaseMyArray(values)
    }
    lazy val defaultRepTo: Rep[BaseMyArray[T]] = BaseMyArray(element[Collection[T]].defaultRepValue)
    lazy val eTo = new BaseMyArrayElem[T](this)
  }
  // 4) constructor and deconstructor
  abstract class BaseMyArrayCompanionAbs extends CompanionBase[BaseMyArrayCompanionAbs] with BaseMyArrayCompanion {
    override def toString = "BaseMyArray"

    def apply[T](values: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[BaseMyArray[T]] =
      mkBaseMyArray(values)
  }
  object BaseMyArrayMatcher {
    def unapply[T](p: Rep[MyArray[T]]) = unmkBaseMyArray(p)
  }
  def BaseMyArray: Rep[BaseMyArrayCompanionAbs]
  implicit def proxyBaseMyArrayCompanion(p: Rep[BaseMyArrayCompanionAbs]): BaseMyArrayCompanionAbs = {
    proxyOps[BaseMyArrayCompanionAbs](p)
  }

  implicit case object BaseMyArrayCompanionElem extends CompanionElem[BaseMyArrayCompanionAbs] {
    lazy val tag = weakTypeTag[BaseMyArrayCompanionAbs]
    protected def getDefaultRep = BaseMyArray
  }

  implicit def proxyBaseMyArray[T](p: Rep[BaseMyArray[T]]): BaseMyArray[T] =
    proxyOps[BaseMyArray[T]](p)

  implicit class ExtendedBaseMyArray[T](p: Rep[BaseMyArray[T]])(implicit eT: Elem[T]) {
    def toData: Rep[BaseMyArrayData[T]] = isoBaseMyArray(eT).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoBaseMyArray[T](implicit eT: Elem[T]): Iso[BaseMyArrayData[T], BaseMyArray[T]] =
    new BaseMyArrayIso[T]

  // 6) smart constructor and deconstructor
  def mkBaseMyArray[T](values: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[BaseMyArray[T]]
  def unmkBaseMyArray[T](p: Rep[MyArray[T]]): Option[(Rep[Collection[T]])]

  // elem for concrete class
  class PairMyArrayElem[A, B](val iso: Iso[PairMyArrayData[A, B], PairMyArray[A, B]])(implicit eA: Elem[A], eB: Elem[B])
    extends MyArrayElem[(A, B), PairMyArray[A, B]]
    with ConcreteElem[PairMyArrayData[A, B], PairMyArray[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(myArrayElement(pairElement(element[A],element[B])))
    override lazy val entityDef = {
      val module = getModules("MyArrays")
      module.concreteSClasses.find(_.name == "PairMyArray").get
    }
    override lazy val tyArgSubst: Map[String, TypeDesc] = {
      Map("A" -> Left(eA), "B" -> Left(eB))
    }

    override def convertMyArray(x: Rep[MyArray[(A, B)]]) = PairMyArray(x.values)
    override def getDefaultRep = super[ConcreteElem].getDefaultRep
    override lazy val tag = {
      implicit val tagA = eA.tag
      implicit val tagB = eB.tag
      weakTypeTag[PairMyArray[A, B]]
    }
  }

  // state representation type
  type PairMyArrayData[A, B] = Collection[(A, B)]

  // 3) Iso for concrete class
  class PairMyArrayIso[A, B](implicit eA: Elem[A], eB: Elem[B])
    extends Iso[PairMyArrayData[A, B], PairMyArray[A, B]] {
    override def from(p: Rep[PairMyArray[A, B]]) =
      p.values
    override def to(p: Rep[Collection[(A, B)]]) = {
      val values = p
      PairMyArray(values)
    }
    lazy val defaultRepTo: Rep[PairMyArray[A, B]] = PairMyArray(element[Collection[(A, B)]].defaultRepValue)
    lazy val eTo = new PairMyArrayElem[A, B](this)
  }
  // 4) constructor and deconstructor
  abstract class PairMyArrayCompanionAbs extends CompanionBase[PairMyArrayCompanionAbs] with PairMyArrayCompanion {
    override def toString = "PairMyArray"

    def apply[A, B](values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]] =
      mkPairMyArray(values)
  }
  object PairMyArrayMatcher {
    def unapply[A, B](p: Rep[MyArray[(A, B)]]) = unmkPairMyArray(p)
  }
  def PairMyArray: Rep[PairMyArrayCompanionAbs]
  implicit def proxyPairMyArrayCompanion(p: Rep[PairMyArrayCompanionAbs]): PairMyArrayCompanionAbs = {
    proxyOps[PairMyArrayCompanionAbs](p)
  }

  implicit case object PairMyArrayCompanionElem extends CompanionElem[PairMyArrayCompanionAbs] {
    lazy val tag = weakTypeTag[PairMyArrayCompanionAbs]
    protected def getDefaultRep = PairMyArray
  }

  implicit def proxyPairMyArray[A, B](p: Rep[PairMyArray[A, B]]): PairMyArray[A, B] =
    proxyOps[PairMyArray[A, B]](p)

  implicit class ExtendedPairMyArray[A, B](p: Rep[PairMyArray[A, B]])(implicit eA: Elem[A], eB: Elem[B]) {
    def toData: Rep[PairMyArrayData[A, B]] = isoPairMyArray(eA, eB).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPairMyArray[A, B](implicit eA: Elem[A], eB: Elem[B]): Iso[PairMyArrayData[A, B], PairMyArray[A, B]] =
    new PairMyArrayIso[A, B]

  // 6) smart constructor and deconstructor
  def mkPairMyArray[A, B](values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]]
  def unmkPairMyArray[A, B](p: Rep[MyArray[(A, B)]]): Option[(Rep[Collection[(A, B)]])]

  registerModule(scalan.meta.ScalanCodegen.loadModule(MyArrays_Module.dump))
}

// Seq -----------------------------------
trait MyArraysSeq extends MyArraysDsl with scalan.ScalanSeq {
  self: ExampleDslSeq =>
  lazy val MyArray: Rep[MyArrayCompanionAbs] = new MyArrayCompanionAbs with UserTypeSeq[MyArrayCompanionAbs] {
    lazy val selfType = element[MyArrayCompanionAbs]
  }

  case class SeqUnitMyArray
      (override val length: Rep[Int])

    extends UnitMyArray(length)
        with UserTypeSeq[UnitMyArray] {
    lazy val selfType = element[UnitMyArray]
  }
  lazy val UnitMyArray = new UnitMyArrayCompanionAbs with UserTypeSeq[UnitMyArrayCompanionAbs] {
    lazy val selfType = element[UnitMyArrayCompanionAbs]
  }

  def mkUnitMyArray
      (length: Rep[Int]): Rep[UnitMyArray] =
      new SeqUnitMyArray(length)
  def unmkUnitMyArray(p: Rep[MyArray[Unit]]) = p match {
    case p: UnitMyArray @unchecked =>
      Some((p.length))
    case _ => None
  }

  case class SeqBaseMyArray[T]
      (override val values: Rep[Collection[T]])
      (implicit eT: Elem[T])
    extends BaseMyArray[T](values)
        with UserTypeSeq[BaseMyArray[T]] {
    lazy val selfType = element[BaseMyArray[T]]
  }
  lazy val BaseMyArray = new BaseMyArrayCompanionAbs with UserTypeSeq[BaseMyArrayCompanionAbs] {
    lazy val selfType = element[BaseMyArrayCompanionAbs]
  }

  def mkBaseMyArray[T]
      (values: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[BaseMyArray[T]] =
      new SeqBaseMyArray[T](values)
  def unmkBaseMyArray[T](p: Rep[MyArray[T]]) = p match {
    case p: BaseMyArray[T] @unchecked =>
      Some((p.values))
    case _ => None
  }

  case class SeqPairMyArray[A, B]
      (override val values: Rep[Collection[(A, B)]])
      (implicit eA: Elem[A], eB: Elem[B])
    extends PairMyArray[A, B](values)
        with UserTypeSeq[PairMyArray[A, B]] {
    lazy val selfType = element[PairMyArray[A, B]]
  }
  lazy val PairMyArray = new PairMyArrayCompanionAbs with UserTypeSeq[PairMyArrayCompanionAbs] {
    lazy val selfType = element[PairMyArrayCompanionAbs]
  }

  def mkPairMyArray[A, B]
      (values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]] =
      new SeqPairMyArray[A, B](values)
  def unmkPairMyArray[A, B](p: Rep[MyArray[(A, B)]]) = p match {
    case p: PairMyArray[A, B] @unchecked =>
      Some((p.values))
    case _ => None
  }
}

// Exp -----------------------------------
trait MyArraysExp extends MyArraysDsl with scalan.ScalanExp {
  self: ExampleDslExp =>
  lazy val MyArray: Rep[MyArrayCompanionAbs] = new MyArrayCompanionAbs with UserTypeDef[MyArrayCompanionAbs] {
    lazy val selfType = element[MyArrayCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  case class ExpUnitMyArray
      (override val length: Rep[Int])

    extends UnitMyArray(length) with UserTypeDef[UnitMyArray] {
    lazy val selfType = element[UnitMyArray]
    override def mirror(t: Transformer) = ExpUnitMyArray(t(length))
  }

  lazy val UnitMyArray: Rep[UnitMyArrayCompanionAbs] = new UnitMyArrayCompanionAbs with UserTypeDef[UnitMyArrayCompanionAbs] {
    lazy val selfType = element[UnitMyArrayCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object UnitMyArrayMethods {
    object eT {
      def unapply(d: Def[_]): Option[Rep[UnitMyArray]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[UnitMyArrayElem] && method.getName == "eT" =>
          Some(receiver).asInstanceOf[Option[Rep[UnitMyArray]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[UnitMyArray]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object values {
      def unapply(d: Def[_]): Option[Rep[UnitMyArray]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[UnitMyArrayElem] && method.getName == "values" =>
          Some(receiver).asInstanceOf[Option[Rep[UnitMyArray]]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[UnitMyArray]] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[UnitMyArray], Rep[Int])] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[UnitMyArrayElem] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[UnitMyArray], Rep[Int])]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[UnitMyArray], Rep[Int])] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object UnitMyArrayCompanionMethods {
  }

  def mkUnitMyArray
    (length: Rep[Int]): Rep[UnitMyArray] =
    new ExpUnitMyArray(length)
  def unmkUnitMyArray(p: Rep[MyArray[Unit]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: UnitMyArrayElem @unchecked =>
      Some((p.asRep[UnitMyArray].length))
    case _ =>
      None
  }

  case class ExpBaseMyArray[T]
      (override val values: Rep[Collection[T]])
      (implicit eT: Elem[T])
    extends BaseMyArray[T](values) with UserTypeDef[BaseMyArray[T]] {
    lazy val selfType = element[BaseMyArray[T]]
    override def mirror(t: Transformer) = ExpBaseMyArray[T](t(values))
  }

  lazy val BaseMyArray: Rep[BaseMyArrayCompanionAbs] = new BaseMyArrayCompanionAbs with UserTypeDef[BaseMyArrayCompanionAbs] {
    lazy val selfType = element[BaseMyArrayCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object BaseMyArrayMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[BaseMyArray[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[BaseMyArrayElem[_]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[BaseMyArray[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[BaseMyArray[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[BaseMyArray[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[BaseMyArrayElem[_]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[BaseMyArray[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[BaseMyArray[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object BaseMyArrayCompanionMethods {
  }

  def mkBaseMyArray[T]
    (values: Rep[Collection[T]])(implicit eT: Elem[T]): Rep[BaseMyArray[T]] =
    new ExpBaseMyArray[T](values)
  def unmkBaseMyArray[T](p: Rep[MyArray[T]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: BaseMyArrayElem[T] @unchecked =>
      Some((p.asRep[BaseMyArray[T]].values))
    case _ =>
      None
  }

  case class ExpPairMyArray[A, B]
      (override val values: Rep[Collection[(A, B)]])
      (implicit eA: Elem[A], eB: Elem[B])
    extends PairMyArray[A, B](values) with UserTypeDef[PairMyArray[A, B]] {
    lazy val selfType = element[PairMyArray[A, B]]
    override def mirror(t: Transformer) = ExpPairMyArray[A, B](t(values))
  }

  lazy val PairMyArray: Rep[PairMyArrayCompanionAbs] = new PairMyArrayCompanionAbs with UserTypeDef[PairMyArrayCompanionAbs] {
    lazy val selfType = element[PairMyArrayCompanionAbs]
    override def mirror(t: Transformer) = this
  }

  object PairMyArrayMethods {
    object arr {
      def unapply(d: Def[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "arr" =>
          Some(receiver).asInstanceOf[Option[Rep[PairMyArray[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object as {
      def unapply(d: Def[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "as" =>
          Some(receiver).asInstanceOf[Option[Rep[PairMyArray[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object bs {
      def unapply(d: Def[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "bs" =>
          Some(receiver).asInstanceOf[Option[Rep[PairMyArray[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[PairMyArray[A, B]], Rep[Int]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[PairMyArray[A, B]], Rep[Int]) forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[PairMyArray[A, B]], Rep[Int]) forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object length {
      def unapply(d: Def[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PairMyArrayElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[PairMyArray[A, B]] forSome {type A; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PairMyArray[A, B]] forSome {type A; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object PairMyArrayCompanionMethods {
    // WARNING: Cannot generate matcher for method `apply`: Method's return type MyArray[(A, B)] is not a Rep
  }

  def mkPairMyArray[A, B]
    (values: Rep[Collection[(A, B)]])(implicit eA: Elem[A], eB: Elem[B]): Rep[PairMyArray[A, B]] =
    new ExpPairMyArray[A, B](values)
  def unmkPairMyArray[A, B](p: Rep[MyArray[(A, B)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairMyArrayElem[A, B] @unchecked =>
      Some((p.asRep[PairMyArray[A, B]].values))
    case _ =>
      None
  }

  object MyArrayMethods {
    object length {
      def unapply(d: Def[_]): Option[Rep[MyArray[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "length" =>
          Some(receiver).asInstanceOf[Option[Rep[MyArray[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MyArray[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object values {
      def unapply(d: Def[_]): Option[Rep[MyArray[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "values" =>
          Some(receiver).asInstanceOf[Option[Rep[MyArray[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[MyArray[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object apply {
      def unapply(d: Def[_]): Option[(Rep[MyArray[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(i, _*), _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "apply" =>
          Some((receiver, i)).asInstanceOf[Option[(Rep[MyArray[T]], Rep[Int]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MyArray[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `map`: Method has function arguments f

    object mapBy {
      def unapply(d: Def[_]): Option[(Rep[MyArray[T]], Rep[T => B]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "mapBy" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[MyArray[T]], Rep[T => B]) forSome {type T; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MyArray[T]], Rep[T => B]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object zip {
      def unapply(d: Def[_]): Option[(Rep[MyArray[T]], MyArr[B]) forSome {type T; type B}] = d match {
        case MethodCall(receiver, method, Seq(ys, _*), _) if receiver.elem.isInstanceOf[MyArrayElem[_, _]] && method.getName == "zip" =>
          Some((receiver, ys)).asInstanceOf[Option[(Rep[MyArray[T]], MyArr[B]) forSome {type T; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[MyArray[T]], MyArr[B]) forSome {type T; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object MyArrayCompanionMethods {
    object apply {
      def unapply(d: Def[_]): Option[Rep[Collection[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(coll, _*), _) if receiver.elem == MyArrayCompanionElem && method.getName == "apply" =>
          Some(coll).asInstanceOf[Option[Rep[Collection[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromArray {
      def unapply(d: Def[_]): Option[Rep[Collection[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(arr, _*), _) if receiver.elem == MyArrayCompanionElem && method.getName == "fromArray" =>
          Some(arr).asInstanceOf[Option[Rep[Collection[T]] forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Collection[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object replicate {
      def unapply(d: Def[_]): Option[(Rep[Int], Rep[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, Seq(len, v, _*), _) if receiver.elem == MyArrayCompanionElem && method.getName == "replicate" =>
          Some((len, v)).asInstanceOf[Option[(Rep[Int], Rep[T]) forSome {type T}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Int], Rep[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object MyArrays_Module {
  val packageName = "scalan.examples"
  val name = "MyArrays"
  val dump = "H4sIAAAAAAAAALVXTWwbRRSe3cRxbEdJKFVRkSAhmIZWELsVqIccIsd1oMhJrGyKkIkqjddjZ8rs7GZnHNkcKsQJwQ1xBKHee+OCVKkXhIQ4cEKAxJlTKUIVUPUAYmb2x+v12kmK8GE0P2/fz/e+92Z8+z5IMRecYyYkkK5YiMMVQ81LjOeNCuWY9zbtZoegK6j1/pkvzU26znQwVwdT+5BdYaQOMt6k0nXCuYEOqiADqYkYt13GwXNVZaFg2oQgk2ObFrBldThsEFSoYsZXq2CyYTd7B+Am0Kpg3rSp6SKOjDKBjCHm708j6REO1xm17m07fRu0IKMoRKLYdSHmwn1hY96T30GO0aM27VkczPqubTvSLSGTxpZjuzwwkRbq9u1msJykUGyAU9Ub8BAWhIl2weAupm3xZc6B5juwjbaEiBSfFA4zRFq7PUetJ6ogy9CBAOiq5RC103UAACIDl5QTK318VkJ8ViQ+eQO5GBL8LpSHNdfu9oD30yYA6DpCxUtHqAg0oApt5j/cM99+aOQsXX7cla6kVYRTQtHCCDaoVAgcv9n5mD147dZlHWTrIItZqcG4C00eTbmPVg5SanPlcwggdNsiW0ujsqWslIRMjBIZ07YcSIUmH8oZkSeCTcylsNyb8bMzAvo0d1AgqnUdLYx3cUS8ijdlSEjt3tmXX/i18pYO9EETGaHSEMR3A6UcpDd7JdeFvVD786O0O6jmYkuw+RC9+tWda7/f3UopA6eaqAU7hL8JSQd53PLN9U1LS/qL5zmYvEYxl1uZbn9MjwkqhHf53m/Nr4tgTw+T4sdwPB4IFSn20w+578+v6WC6rqpmg8B2XeSFVQiytt2yTXkdTNuHyPVO0oeQyFkiL9J+2H62ojBPCJg5WBxZ3w6SOVhVtaQFAOS8ctiyKcpv1PJ/Gd9+cluy3QUz3olX8P/gy3//PNviqhA4mCKItvm+cmqOgwnRKXw85PgkB1pR7F6liZhnPcWGbaEnlh7g67c+4gpdrTvYMLYbN0SFrqrvnh0DdNC4/qwX9T/O/vi5DjICzwbmFnTyxWOW2/9YQiACjQBrtuw3bUWQ4uBhThLVrw2F1xCA4bAg8nA6Il6Oer3Q71VPqemw6ohIThv0Ilad/q62m+yRZyUi+3RIMGVH0OVQFikbQRexky2H7fhYNqOWl9V4YSzKF2Mor0OGToByRHwkytH4tVj8OtoNTE/Koj8yxmEXF8J+88zoTilId2aneprcX7urg9QbINUSbYRVQaphd2gzYLN4NHDU5evBnjbIZsFe6EIrZK/6LYJ+vDGPj8OgMcXroN2OQ9Ardx5d/+C91x3VCYYujBhgpT7ucrl+JEfGsFMOaydlZvPxHTopdS/FqFuD2B2mbnA8n0TTiCcxYkeUPSaxS+OIXYp7mKBgfZyCYSCHEViIfLOXyNZIdpIFhs1EdMbTk9lBuIXle/E/NM1oNsZQISdvlQ1oYdK7mGwtUftglhM54cXuDCo6GsrkaD7ty/iC0wEYHMz5lY+6UFyj/i2wLFrC0oiWYPi3qbjSbz78bOvCd1/8ol58WXkvi3cKDf90RF96sbKteNbEn4iIx4Jk8qpW3v4LMpbGjNQNAAA="
}
}

