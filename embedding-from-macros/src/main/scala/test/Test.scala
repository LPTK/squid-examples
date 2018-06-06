package test

import squid.ir.{OnlineOptimizer,SimpleAST,TopDownTransformer}
import squid.quasi.{ModularEmbedding,MetaBases}

import scala.reflect.macros.whitebox

object Test2 {
  
  import scala.language.experimental.macros
  
  def test[A](a: A): A = macro testImpl[A]
  
  object IR extends SimpleAST with OnlineOptimizer {
    def pipeline = Tr.pipeline
  }
  import IR.Predef._
  
  //object Tr extends squid.ir.SimpleRuleBasedTransformer with EB.SelfTransformer with TopDownTransformer {
  // ^ if not online, make the transformer a TopDownTransformer or BottomUpTransformer
  object Tr extends squid.ir.SimpleRuleBasedTransformer with IR.SelfTransformer {
    rewrite {
      case code"123" => code"42"
    }
  }
  
  def testImpl[A:c.WeakTypeTag](c: whitebox.Context)(a: c.Tree): c.Tree = {
    import c.universe._
    
    object ME extends ModularEmbedding[c.universe.type, IR.type](c.universe, IR, 
      debug = str => println(str)) // change 'debug' to avoid polluting compile-time stdout
    val pgrm = IR.Code[A,squid.utils.Bottom](ME(a))
    
    println(s"FIRST >> ${pgrm}")
    val pgrm2 = pgrm transformWith Tr // no effect since Tr is now applied online
    
    println(s"SECOND >> ${pgrm2}")
    val msg = s"[At compile time:]\n\tOriginal program: $pgrm\n\tTransformed program: $pgrm2"
    
    // putting things back into Scala (untyped trees):
    object MBM extends MetaBases {
      val u: c.universe.type = c.universe
      def freshName(hint: String) = c.freshName(TermName(hint))
    }
    val MB = new MBM.ScalaReflectionBase
    val res = IR.scalaTreeIn(MBM)(MB, pgrm2.rep, base.DefaultExtrudedHandler)
    
    println("Generated Scala tree: " + showCode(res))
    
    q"println($msg); $res"
    
  }
  
}
