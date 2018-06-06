package relation

import squid.ir._
import squid.utils.&
import squid.lang.{InspectableBase, ScalaCore}
import squid.lib.{MutVar, transparencyPropagating}
import squid.quasi.{embed, phase}
import squid.anf.transfo.TupleNormalizer

import scala.collection.mutable.{HashMap, ArrayBuffer}

object RelationInliner extends RelationDSL.Lowering('RelRemove)

object RelationCtorInliner extends RelationDSL.SelfTransformer with SimpleRuleBasedTransformer {
  import RelationDSL.Predef._
  /*_*/
  rewrite {
    case c"val $r = new Relation($sch, $under); $body: $t" =>
      val newBody = body rewrite {
        case c"$$r.schema" => sch
        case c"$$r.underlying" => under
      }
      newBody.subs(r) ~> Abort()

  }
}

object RelationLowering extends RelationDSL.TransformerWrapper(RelationInliner, RelationCtorInliner) with BottomUpTransformer with FixPointTransformer

object SchemaSpecialization extends RelationDSL.SelfTransformer with FixPointRuleBasedTransformer with TopDownTransformer with FixPointTransformer {
  import RelationDSL.Predef._
  rewrite {
    case c"new Schema(List[String]($columns*))" =>
      c"Schema($columns*)"
  }
  rewrite {
    case c"val $s = Schema($columns*); $body: $t" =>
      //println(s"transforming $columns, $body")
      val newBody = body fix_rewrite {
        case c"($r: Row).getField($$s, $name)" if columns.contains(name) =>
          val index = columns.zipWithIndex.find(_._1 == name).get._2
          //println(s"found $name in $columns, index: $index");
          c"$r.getValue(${Const(index)})"
        case c"$$s.columns" => c"List($columns*)"
        case c"$$s.indicesOf(List[String]($columns2*))" =>
          val cols = columns:Seq[Code[String,Nothing]] // TODO better interface
          val columnIndexMap = cols.zipWithIndex.toMap
          val indices = columns2.map(x => columnIndexMap(x)).map(x => Const(x))
          c"List($indices*)"
      }
      newBody.subs(s) ~> Abort()
      //c"val s = Schema($columns*); $newBody"
  }

  rewrite {
    case c"List[$t]($elems*).zip($l2: List[$t2])" =>
      val newElems = elems.zipWithIndex.map(x => c"(${x._1}, $l2(${Const(x._2)}))")
      c"List($newElems*)"
  }
  rewrite {
    case c"List[$t]($elems*).zipWithIndex" =>
      val newElems = elems.zipWithIndex.map(x => c"(${x._1}, ${Const(x._2)})")
      c"List($newElems*)"
  }
  rewrite {
    case c"List[$t]($elems*).map($f: t => $t2)" =>
      val newElems = elems.map(x => c"$f($x)")
      c"List($newElems*)"
  }
  rewrite {
    case c"List[$t]($elems*).size" =>
      Const(elems.size)
  }
  rewrite {
    case c"List[$t]($elems1*) ++ List[t]($elems2*)" =>
      val newElems = elems1 ++ elems2
      c"List($newElems*)"
  }
}

object ListFusion extends RelationDSL.TransformerWrapper(ListFusionTransformers.ListToStream, ListFusionTransformers.StreamFusion, ListFusionTransformers.StreamLowering) with BottomUpTransformer

@embed
class Stream[T](val consume: (T => Unit) => Unit) {
  @phase('StreamInline)
  def map[S](f: T => S): Stream[S] = new Stream(k => foreach(e => k(f(e))))
  @phase('StreamInline)
  def filter(p: T => Boolean): Stream[T] = new Stream(k => foreach(e => if(p(e)) k(e)))
  @phase('StreamInline)
  def foreach(f: T => Unit): Unit = consume(f)
  def toList: List[T] = ???
}
object Stream {
  @phase('StreamInline)
  def fromList[T](l: List[T]): Stream[T] = new Stream(k => l.foreach(k))
}

object ListFusionTransformers {
  object ListToStream extends RelationDSL.SelfTransformer with FixPointRuleBasedTransformer with TopDownTransformer with FixPointTransformer {
    import RelationDSL.Predef._

    rewrite {
      case c"($l: List[$t]).map($f: t => $t2)" =>
        c"Stream.fromList($l).map($f).toList"
    }

    rewrite {
      case c"($l: List[$t]).filter($p)" =>
        c"Stream.fromList($l).filter($p).toList"
    }

    rewrite {
      case c"($l: List[$t]).foreach($f: t => Unit)" =>
        c"Stream.fromList($l).foreach($f)"
    }
  }

  object StreamFusion extends RelationDSL.SelfTransformer with FixPointRuleBasedTransformer with TopDownTransformer with FixPointTransformer {
    import RelationDSL.Predef._

    rewrite {
      case c"Stream.fromList(($s: Stream[$t]).toList)" =>
        c"$s"
    }
  }

  object StreamInliner extends RelationDSL.Lowering('StreamInline)


  object StreamCtorInliner extends RelationDSL.SelfTransformer with SimpleRuleBasedTransformer {
    import RelationDSL.Predef._
    rewrite {
      case c"val $s = new Stream($consume: ($t => Unit) => Unit); $body: $t2" =>
        val newBody = body rewrite {
          case c"$$s.consume" => consume
        }
        newBody.subs(s) ~> {Abort()}

    }
  }

  object StreamLowering extends RelationDSL.TransformerWrapper(StreamInliner, StreamCtorInliner) with BottomUpTransformer with FixPointTransformer
}

class TupledRow(val tup: Product) {
  @transparencyPropagating
  def toRow: Row = ???
  @transparencyPropagating
  def getElem(idx: Int): String = ???
}

object TupledRow {
  @transparencyPropagating
  def fromRow(r: Row): TupledRow = ???
  @transparencyPropagating
  def apply(p: Product): TupledRow = ???
}

object RowLayout extends RelationDSL.TransformerWrapper(
    RowLayoutTransformers.RowToTupledRow
    , RowLayoutTransformers.TupledRowFusion
    , RowLayoutTransformers.TupledRowLowering
  ) with TopDownTransformer

// TODO move it to the core optimizations.
trait TupleNNormalizer extends TupleNormalizer { self =>
  //val base: InspectableBase with ScalaCore
  val base: RelationDSL.type
  import base.Predef._

  rewrite {
    case c"($a:$ta,$b:$tb,$c:$tc)._1" => c"$a"
    case c"($a:$ta,$b:$tb,$c:$tc)._2" => c"$b"
    case c"($a:$ta,$b:$tb,$c:$tc)._3" => c"$c"
    case c"($a:$ta,$b:$tb,$c:$tc,$d:$td)._1" => c"$a"
    case c"($a:$ta,$b:$tb,$c:$tc,$d:$td)._2" => c"$b"
    case c"($a:$ta,$b:$tb,$c:$tc,$d:$td)._3" => c"$c"
    case c"($a:$ta,$b:$tb,$c:$tc,$d:$td)._4" => c"$d"
  }

}

object TupleProcessing {
  import RelationDSL.Predef._
  import scala.reflect.runtime.{ universe => ru }
  import ru.{ Type => ScalaType }

  object TupleType { // TODO complete with all tuple types!
    import ru._

    val Tup2Sym = typeOf[(Any, Any)].typeSymbol.asType
    val Tup3Sym = typeOf[(Any, Any, Any)].typeSymbol.asType
    val Tup4Sym = typeOf[(Any, Any, Any, Any)].typeSymbol.asType

    val tupleSyms = List(Tup2Sym, Tup3Sym, Tup4Sym)

    val scalaPackage = Tup2Sym.owner.asType.toType

    def apply(params: ScalaType*) = internal.typeRef(scalaPackage, symbol(params.size), params.toList)

    def symbol(arity: Int) = (arity match {
      case 2 => Tup2Sym
      case 3 => Tup3Sym
      case 4 => Tup4Sym
      case _ => throw new UnsupportedOperationException(s"Tuple type of arity $arity not between 2 and 4")
    }).asType

    def unapply(tpe: ScalaType): Option[(Int, List[ScalaType])] = {
      tpe match {
        case TypeRef(pre, sym, args) if pre == scalaPackage && tupleSyms.contains(sym) =>
          Some(args.length -> args)
        case _ => None
      }
    }
  }
  def scalaTypeToCodeType(tpe: ScalaType): CodeType[_] =
    RelationDSL.CodeType(RelationDSL.TypeRep(tpe))

  def getTupleType(arity: Int): CodeType[_] = {
    arity match {
      case 2 => codeTypeOf[(String, String)]
      case 3 => codeTypeOf[(String, String, String)]
      case 4 => codeTypeOf[(String, String, String, String)]
      case _ => throw new Exception(s"Does not support getting the type a tuple of $arity elements.")
    }
  }

  def getTupleTypeArgs(tp: CodeType[_]): List[CodeType[_]] =
    tp.rep.tpe match {
      case TupleType(_, list) => list.map(scalaTypeToCodeType)
      case _ => throw new Exception(s"Couldn't retrieve tuple type args for type $tp")
    }

  def constructTupleType(types: List[CodeType[_]]): CodeType[_] = {
    scalaTypeToCodeType(TupleType(types.map(_.rep.tpe): _*))
  }

  def constructTuple[R: CodeType, C](f: Int => Code[R, C], arity: Int): Code[_, C] = {
    arity match {
      case 2 => c"(${f(0)}, ${f(1)})"
      case 3 => c"(${f(0)}, ${f(1)}, ${f(2)})"
      case 4 => c"(${f(0)}, ${f(1)}, ${f(2)}, ${f(3)})"
      case _ => throw new Exception(s"Does not support the construction of a tuple of $arity elements.")
    }
  }

  def constructTupleFromList[C <: AnyRef](elems: Code[List[String], C], arity: Int): Code[_, C] = {
    constructTuple((idx: Int) => c"$elems(${Const(idx)})", arity)
  }

  def getTupleArity(tup:Code[Any,_]): Int = {
    tup match {
      case c"$tup: ($ta,$tb)" => 2
      case c"$tup: ($ta,$tb,$tc)" => 3
      case c"$tup: ($ta,$tb,$tc,$td)" => 4
      case _ => throw new Exception(s"Does not support getting the arity of the tuple `$tup`, ${tup.typ}.")
    }
  }

  def getTupleArity[T: CodeType]: Int = {
    codeTypeOf[T] match {
      case x if x == getTupleType(2) => 2
      case x if x == getTupleType(3) => 3
      case x if x == getTupleType(4) => 4
      case x => throw new Exception(s"Does not support getting the arity of the tuple type `$x`.")
    }
  }

  def isTupleType[T: CodeType]: Boolean =
    try {
      getTupleArity[T] > 1
    } catch {
      case x: Exception => false
    }

  def projectTuple[C, T, R](tup:Code[T,C], idx:Int): Code[R, C] = {
    val res = tup match {
      case c"$tup: ($ta,$tb)" => idx match {
        case 0 => c"$tup._1"
        case 1 => c"$tup._2"
      }
      case c"$tup: ($ta,$tb,$tc)" => idx match {
        case 0 => c"$tup._1"
        case 1 => c"$tup._2"
        case 2 => c"$tup._3"
      }
      case c"$tup: ($ta,$tb,$tc, $td)" => idx match {
        case 0 => c"$tup._1"
        case 1 => c"$tup._2"
        case 2 => c"$tup._3"
        case 3 => c"$tup._4"
      }
      case c"$tup: Product" => throw new Exception(s"Does not support the projection of the ${idx}th element of the tuple `$tup`.")
    }
    c"$res".asInstanceOf[Code[R, C]]
  }

  implicit class TupleRepOps[T, C](tup: Code[T, C]) {
    def project[R](idx: Int) = projectTuple[C, T, R](tup, idx)
  }
}

object RowLayoutTransformers {
  import RelationDSL.Predef._
  import TupleProcessing._

  object RowToTupledRow extends RelationDSL.SelfTransformer with SimpleRuleBasedTransformer with BottomUpTransformer with FixPointTransformer {


    def listRewrite[T:CodeType,C](list: Variable[MutVar[List[Row]]])(body: Code[T,C & list.Ctx]): Code[T,C] = {
      var size: Int = -1
      body analyse {
        case c"$$list := ($$list.!).::(Row($_, ${Const(s)}))" =>
          size = s
      }
      getTupleType(size) match { case tupType: CodeType[tp] =>
        val newList = Variable[MutVar[List[tupType.Typ]]]
        val body0 = body rewrite {
          case c"$$list := ($$list.!).::(Row($elems, ${Const(s)}))" =>
            c"$newList := ($newList.!).::(${constructTupleFromList(elems, s)}.asInstanceOf[$tupType])"
          case c"val $listVar = $$list.!; $subBody: $tp2" =>
            val subBody2 = subBody rewrite {
              case c"$$listVar.foreach[$t](x => $fbody)" => c"($newList.!) foreach {e => val x = TupledRow(e.asInstanceOf[Product]).toRow; $fbody}"
            }
            subBody2.subs(listVar) ~> { Abort() }
        }
        val body1 = body0.subs(list) ~> {System.err.println(s"list body $tupType::$body0"); throw RewriteAbort()}
        c"val $newList = MutVar(Nil); $body1"
      }}

    // TODO maybe move it to the core compilation facilities.
    implicit class IRCastingOps[T: CodeType, C](e: Code[T, C]) {
      def __inlinedCast[T2: CodeType]: Code[T2, C] = e.asInstanceOf[Code[T2, C]]
    }

    def hashMapRewrite[T:CodeType,C](hm: Variable[HashMap[String, Row]])(body: Code[T,C & hm.Ctx]): Code[T,C] = {
      var size: Int = -1
      body analyse {
        case c"$$hm += (($_: String, TupledRow((($tup): scala.Product)).toRow))" =>
          size = getTupleArity(tup)
      }
      if (size == -1) throw RewriteAbort()
      getTupleType(size) match { case tupTypeVal: CodeType[tupType] =>
        val newHm = Variable[HashMap[String, tupType]]
        val body0 = body rewrite {
          case c"$$hm += (($key: String, TupledRow($tup: scala.Product).toRow)); ()" =>
            c"$newHm += (($key, ($tup.asInstanceOf[tupType]))); ()"
          case c"$$hm.contains($key)" => c"$newHm.contains($key)"
          case c"$$hm.apply($key: String)" => c"TupledRow(${c"$newHm.apply($key)".__inlinedCast[Product]}).toRow"
        }
        val body1 = body0.subs(hm) ~> {throw RewriteAbort()}
        c"val $newHm = new HashMap[String, tupType]; $body1"
      }}


    rewrite {
      case c"($r: Row).getValue($idx)" =>
        c"TupledRow.fromRow($r).getElem($idx)"
      case c"val $list: MutVar[List[Row]] = MutVar(Nil); $body: $t2" =>
        listRewrite[t2.Typ,list.OuterCtx](list)(body)
      case c"val $hm: HashMap[String, Row] = new HashMap[String, Row]; $body: $t2" =>
        hashMapRewrite[t2.Typ,hm.OuterCtx](hm)(body)
    }

  }

  object TupledRowFusion extends RelationDSL.SelfTransformer with SimpleRuleBasedTransformer with BottomUpTransformer with FixPointTransformer {
    rewrite {
      case c"TupledRow.fromRow(($t: TupledRow).toRow)" =>
        t
      case c"TupledRow($e1: Product).toRow.append(TupledRow($e2: Product).toRow)" =>
        val n1 = getTupleArity(e1)
        val n2 = getTupleArity(e2)
        val elems = constructTuple((i: Int) => if (i < n1) projectTuple(e1, i) else projectTuple(e2, i - n1), n1 + n2).asInstanceOf[Code[Product, e1.Ctx]]
        c"TupledRow($elems).toRow"
    }

  }

  object TupledRowLowering extends RelationDSL.SelfTransformer with SimpleRuleBasedTransformer with BottomUpTransformer with FixPointTransformer {
    rewrite {
      case c"TupledRow($e: Product).getElem(${Const(idx)})" =>
        projectTuple(e, idx)
      case c"TupledRow($e: Product).toRow" =>
        val arity = getTupleArity(e)
        val elems = (0 until arity).map(i => projectTuple(e, i))
        c"Row(List[String]($elems*), ${Const(arity)})"
    }
  }
}


object ListToArrayBuffer extends RelationDSL.SelfTransformer with SimpleRuleBasedTransformer with BottomUpTransformer with FixPointTransformer {
  import RelationDSL.Predef._

  def listRewrite[T:CodeType,R:CodeType,C](list: Variable[MutVar[List[R]]])(body: Code[T,C & list.Ctx]): Code[T,C] = {
    val ab = Variable[ArrayBuffer[R]]
    val body0 = body rewrite {
      case c"$$list := ($$list.!).::($e: R)" =>
        c"$ab += $e; ()"
      case c"val $listVar = $$list.!; $subBody: $tp2" =>
        val subBody2 = subBody rewrite {
          case c"$$listVar.foreach[$t](x => $fbody)" => c"for(i <- 0 until $ab.length) { val x = $ab(i); $fbody}"
        }
        subBody2.subs(listVar) ~> {System.err.println(s"inside list var access $subBody"); Abort()}
    }
    val body1 = body0.subs(list) ~> {System.err.println(s"list body $body0"); throw RewriteAbort()}
    c"val $ab = new ArrayBuffer[R](); $body1"
  }
  rewrite {
    case c"val $list: MutVar[List[$t1]] = MutVar(Nil); $body: $t2" =>
      listRewrite[t2.Typ,t1.Typ,list.OuterCtx](list)(body)
  }
}

@embed
class OpenHashMap[K, V](val maxSize: Int, var count: Int, val positions: Array[Int],
                       val keys: ArrayBuffer[K], val values: ArrayBuffer[V]) {

  @phase('OpenHashMapInline)
  def init(): Unit = {
    for(i <- 0 until maxSize) {
      positions(i) = -1
    }
  }
  @phase('OpenHashMapInline)
  def hashFunction(k: K): Int =
    k.hashCode()
  @phase('OpenHashMapInline)
  def findPosition(k: K): Int = {
    var h = hashFunction(k) % maxSize
    while(positions(h) != -1 && keys(positions(h)) != k) {
      h = (h + 1) % maxSize
    }
    h
  }
  @phase('OpenHashMapInline)
  def +=(pair: (K, V)): Unit = {
    val k = pair._1
    val v = pair._2
    val pos = findPosition(k)
    if(positions(pos) != -1) {
      val index = count
      count += 1
      positions(pos) = index
      keys(index) = k
      values(index) = v
    }
  }
  @phase('OpenHashMapInline)
  def contains(k: K): Boolean = {
    val pos = findPosition(k)
    positions(pos) != -1
  }
  @phase('OpenHashMapInline)
  def apply(k: K): V = {
    val pos = findPosition(k)
    values(positions(pos))
  }
}
object OpenHashMap {
  @phase('OpenHashMapInline)
  def apply[K, V](): OpenHashMap[K, V] = {
    val maxSize = 1 << 12
    val ohm = new OpenHashMap[K, V](maxSize, 0, new Array[Int](maxSize), ArrayBuffer.fill[K](maxSize)(null.asInstanceOf[K]), ArrayBuffer.fill[V](maxSize)(null.asInstanceOf[V]))
    ohm.init()
    ohm
  }
}

object HashMapToOpenHashMap extends RelationDSL.SelfTransformer with SimpleRuleBasedTransformer with BottomUpTransformer with FixPointTransformer {
  import RelationDSL.Predef._

  def tableSize = c"1 << 12"

  def hashMapRewrite[T:CodeType, K: CodeType, R:CodeType,C](hm: Variable[HashMap[K, R]])(body: Code[T,C & hm.Ctx]): Code[T,C] = {
    val ohm = Variable[OpenHashMap[K, R]]
    val body0 = body rewrite {
      case c"$$hm += (($key: K, $value: R)); ()" =>
        c"$ohm += (($key, $value))"
      case c"$$hm.contains($key)" => c"$ohm.contains($key)"
      case c"$$hm.apply($key)" => c"$ohm.apply($key)"
    }
    val body1 = body0.subs(hm) ~> (throw RewriteAbort())
    c"val $ohm = OpenHashMap[K, R](); $body1"
  }


  rewrite {
    case c"val $hm: HashMap[$t0, $t1] = new HashMap[t0, t1]; $body: $t2" =>
      hashMapRewrite[t2.Typ,t0.Typ,t1.Typ,hm.OuterCtx](hm)(body)
  }

}

object OpenHashMapInliner extends RelationDSL.Lowering('OpenHashMapInline)

object OpenHashMapCtorInliner extends RelationDSL.SelfTransformer with SimpleRuleBasedTransformer {
  import RelationDSL.Predef._
  rewrite {
    case c"val $ohm = new OpenHashMap[$k, $v]($size, $c, $positions, $keys, $values); $body: $t" =>
      val count = Variable[MutVar[Int]]
      val body0 = body rewrite {
        case c"$$ohm.maxSize" => size
        case c"$$ohm.count" => c"$count.!"
        case c"$$ohm.count = $v" => c"$count := $v"
        case c"$$ohm.positions" => positions
        case c"$$ohm.keys" => keys
        case c"$$ohm.values" => values
      }
      val body1 = body0.subs(ohm) ~> Abort()
      c"val $count = MutVar($c); $body1"
  }
}


object OpenHashMapLowering extends RelationDSL.TransformerWrapper(OpenHashMapInliner, OpenHashMapCtorInliner) with BottomUpTransformer with FixPointTransformer

object ArrayBufferColumnar extends RelationDSL.SelfTransformer with SimpleRuleBasedTransformer with BottomUpTransformer with FixPointTransformer {
  import RelationDSL.Predef._
  import TupleProcessing._

  type CA[R, C] = C {val ab: ArrayBuffer[R]}

  def arrayBufferRewrite[T:CodeType, R:CodeType,C](ab: Variable[ArrayBuffer[R]])(body: Code[T,C & ab.Ctx], init: Option[Code[Int, C]]): Code[T,C] = {
    val size = getTupleArity[R]
    val tpArgs = (for (i <- 0 until size) yield codeTypeOf[ArrayBuffer[String]]).toList
    constructTupleType(tpArgs) match {
      case tupType: CodeType[tp] =>
        val abt = Variable[tupType.Typ]
        val body0 = body rewrite {
          case c"$$ab.length" =>
            c"${abt.toCode.project[ArrayBuffer[String]](0)}.length"
          case c"$$ab += ($e: R); ()" =>
            (0 until size).foldRight(c"()".asInstanceOf[Code[Unit, C]])((cur, acc) => c"${abt.toCode.project[ArrayBuffer[String]](cur)} += ${projectTuple(e, cur)}; $acc".asInstanceOf[Code[Unit, C]])
          case c"$$ab($i) = ($e: R)" =>
            (0 until size).foldRight(c"()".asInstanceOf[Code[Unit, C]])((cur, acc) => c"${abt.toCode.project[ArrayBuffer[String]](cur)}($i) = ${projectTuple(e, cur)}; $acc".asInstanceOf[Code[Unit, C]])
          case c"$$ab($i)" =>
            constructTuple(idx => c"${abt.toCode.project[ArrayBuffer[String]](idx)}($i)", size).asInstanceOf[Code[R, C]]
        }
        val body1 = body0.subs(ab) ~> {System.err.println(s"list body $body0"); throw RewriteAbort()}
        c"val $abt = ${constructTuple(idx => {init match {
          case None => c"new ArrayBuffer[String]()"
          case Some(s) => c"""ArrayBuffer.fill[String]($s)("")"""
        }}, size).asInstanceOf[Code[tp, C]]}; $body1"
    }
  }
  rewrite {
    case c"val $ab: ArrayBuffer[$t1] = new ArrayBuffer[t1]; $body: $t2" if isTupleType(t1) =>
      arrayBufferRewrite[t2.Typ,t1.Typ,ab.OuterCtx](ab)(body, None)
    case c"val $ab: ArrayBuffer[$t1] = ArrayBuffer.fill[t1]($size)($v); $body: $t2" if isTupleType(t1) =>
      arrayBufferRewrite[t2.Typ,t1.Typ,ab.OuterCtx](ab)(body, Some(size))
  }
}
