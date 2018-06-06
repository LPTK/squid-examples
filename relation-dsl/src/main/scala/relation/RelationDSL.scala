package relation

import squid.anf.transfo.IdiomsNormalizer
import squid.ir._
import squid.lang.ScalaCore
//
//object RelationDSL extends SimpleANF with ScalaCore with Relation.Lang with Row.Lang with Schema.Lang with ClassEmbedder {
//  embed(Relation)
//}

object RelationDSL extends SimpleANF with ClassEmbedder with SimpleEffects with StandardEffects with OnlineOptimizer
  with CurryEncoding.ApplicationNormalizer with IdiomsNormalizer with TupleNNormalizer with ScalaCore {
  //override val Predef = new Predef[squid.quasi.DefaultQuasiConfig] {
  //  type IR[+T,-C] = Code[T,C]
  //  type IRType[T] = CodeType[T]
  //  val IRType = CodeType
  //  def irTypeOf[T: CodeType] = implicitly[CodeType[T]]
  //}
  override val Predef = new Predef[squid.quasi.DefaultQuasiConfig] with LegacyPredef
  
  embed(Relation)
  embed(Stream)
  embed(OpenHashMap)
}





