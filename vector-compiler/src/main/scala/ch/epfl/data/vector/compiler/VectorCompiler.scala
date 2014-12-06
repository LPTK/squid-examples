package ch.epfl.data
package vector
package compiler

import pardis.types._
import pardis.ir._
import pardis.optimization._
import pardis.compiler._
import deep._
import prettyprinter._

class VectorCompiler(val DSL: VectorDSL) extends Compiler[VectorDSL] {
  pipeline += DCE

  val codeGenerator = new VectorScalaGenerator
}
