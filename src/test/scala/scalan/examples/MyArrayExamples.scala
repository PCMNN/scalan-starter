package scalan.examples

import scalan.primitives.PrimitiveExamples

trait MyArrayExamples extends ExampleDsl with PrimitiveExamples {
  lazy val fromArray = fun { xs: Arr[Int] => MyArray(Collection(xs)) }
  lazy val fromArrayOfPairs = fun { xs: Arr[(Int,Float)] => MyArray(Collection(xs)) }
  lazy val fromAndTo = fun { xs: Arr[(Int,Float)] =>
    val ps = MyArray(PairCollectionAOS(CollectionOverArray(xs)).convertTo[Collection[(Int,Float)]])
    val arr = ps.values.convertTo[PairCollection[Int,Float]].arr
    arr//(arr.map(_._1), arr.map(_._2))
  }

  lazy val mapped = fun {(xs: MyArr[Int]) => xs.mapBy(inc) }
  lazy val zippedMap = fun {(xs: MyArr[Int]) => (xs zip xs).mapBy(tupled) }
  lazy val mapped2 = fun {(xs: MyArr[Int]) => xs.mapBy(inc2) }

  lazy val splitMap = fun {(xs: MyArr[Int]) => Pair(xs.mapBy(inc), xs.mapBy(inc2)) }
  lazy val splitMap2 = fun {(xs: MyArr[Int]) => Pair(xs.mapBy(inc_times), xs.mapBy(inc2)) }
  lazy val mapInc3Times = fun {(xs: MyArr[Int]) => Tuple(xs.mapBy(inc), xs.mapBy(inc), xs.mapBy(inc)) }
  lazy val splitMap3 = fun {(xs: MyArr[Int]) => Tuple(xs.mapBy(inc), xs.mapBy(inc2), xs.mapBy(inc_times)) }
  lazy val splitMapMap = fun {(xs: MyArr[Int]) => Pair(xs.mapBy(inc), xs.mapBy(inc2).mapBy(inc_times)) }

  lazy val mapScalar = fun {(xs: MyArr[Int]) => xs.mapBy(scalar) }
  lazy val mapArrays = fun { xs: Arr[Int] => xs.mapBy(inc) }

  lazy val expBaseArraysInIf = fun { xs: Arr[Int] =>
    val ys = BaseMyArray(Collection(xs))
    val zs = BaseMyArray(Collection(xs).map { x => x + 1 })
    val res = IF (xs.length > 10) THEN { ys } ELSE { zs }
    res
  }
  lazy val expBaseArraysInIfSpec = fun { xs: Arr[Int] =>
    expBaseArraysInIf(xs).values
  }

  lazy val expPairArraysInIf = fun { xs: Arr[Int] =>
    val pairs1 = PairMyArray(BaseMyArray(Collection(xs)), BaseMyArray(Collection(xs)))
    val ys = xs.map { x => x + 1 }
    val pairs2 = PairMyArray(BaseMyArray(Collection(ys)), BaseMyArray(Collection(xs)))
    val res = IF (xs.length > 10) THEN { pairs1 } ELSE { pairs2 }
    res.values.arr
  }
  lazy val expPairArraysInIfSpec = fun { xs: Arr[Int] =>
    expPairArraysInIf(xs)
  }

  lazy val expPairArraysInIfDiffTypes = fun { xs: Arr[(Int,Int)] =>
    val ys = xs.map { case Pair(x, y) => x + y }
    val pairs1 = PairMyArray(BaseMyArray(Collection(ys)), BaseMyArray(Collection(ys)))
    val pairs2 = BaseMyArray(Collection(xs))
    val res = IF (xs.length > 10) THEN pairs1 ELSE pairs2
    res.values.arr
  }
  lazy val expPairArraysInIfDiffTypesSpec = fun { xs: Arr[(Int,Int)] =>
    expPairArraysInIfDiffTypes(xs)
  }

  lazy val pairInIf = fun { in: Arr[Int] =>
    val xs = BaseMyArray(Collection(in))
    val ys = xs.map { x => x + 1 }
    val res = IF (xs.length > 10) THEN { Pair(ys, xs) } ELSE { Pair(xs,xs) }
    res._1
  }
  lazy val pairInIfSpec = fun { xs: Arr[Int] =>
    pairInIf(xs)//.values
  }

  lazy val nestedPairInIf = fun { in: Arr[Int] =>
    val xs: MyArr[Int] = BaseMyArray(Collection(in))
    val ys = xs.map { x => x + 1 }
    val res = IF (xs.length > 10) THEN { Pair(Pair(xs, ys), xs) } ELSE { Pair(Pair(xs,xs),ys) }
    Pair(res._1._2, res._2)
  }
  lazy val nestedPairInIfSpec = fun { xs: Arr[Int] =>
    val res = nestedPairInIf(xs)
    (res._1.values, res._2.values)
  }

}
