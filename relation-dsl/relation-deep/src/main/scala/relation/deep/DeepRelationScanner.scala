/* Generated by Purgatory 2014-2016 */

package relation.deep

import ch.epfl.data.sc.pardis
import pardis.ir._
import pardis.types.PardisTypeImplicits._
import pardis.effects._
import pardis.deep._
import pardis.deep.scalalib._
import pardis.deep.scalalib.collection._
import pardis.deep.scalalib.io._

import ch.epfl.data.sc.pardis.quasi.anf.{ BaseExt, BaseExtIR }
import ch.epfl.data.sc.pardis.quasi.TypeParameters.MaybeParamTag

trait RelationScannerOps extends Base  {  
  // Type representation
  val RelationScannerType = RelationScannerIRs.RelationScannerType
  implicit val typeRelationScanner: TypeRep[RelationScanner] = RelationScannerType
  implicit class RelationScannerRep(self : Rep[RelationScanner]) {
     def next_int() : Rep[Int] = relationScannerNext_int(self)
     def next_double() : Rep[Double] = relationScannerNext_double(self)
     def next_char() : Rep[Char] = relationScannerNext_char(self)
     def next(buf : Rep[Array[Byte]])(implicit overload1 : Overloaded1) : Rep[Int] = relationScannerNext1(self, buf)
     def next(buf : Rep[Array[Byte]], offset : Rep[Int])(implicit overload2 : Overloaded2) : Rep[Int] = relationScannerNext2(self, buf, offset)
     def next_string() : Rep[String] = relationScannerNext_string(self)
     def next_date : Rep[Int] = relationScannerNext_date(self)
     def hasNext() : Rep[Boolean] = relationScannerHasNext(self)
     def delimiter : Rep[Char] = relationScanner_Field_Delimiter(self)
     def filename : Rep[String] = relationScanner_Field_Filename(self)
  }
  object RelationScanner {
     def getNumLinesInFile(filePath : Rep[String]) : Rep[Int] = relationScannerGetNumLinesInFileObject(filePath)
  }
  // constructors
   def __newRelationScanner(filename : Rep[String], delimiter : Rep[Char]) : Rep[RelationScanner] = relationScannerNew(filename, delimiter)
  // IR defs
  val RelationScannerNew = RelationScannerIRs.RelationScannerNew
  type RelationScannerNew = RelationScannerIRs.RelationScannerNew
  val RelationScannerNext_int = RelationScannerIRs.RelationScannerNext_int
  type RelationScannerNext_int = RelationScannerIRs.RelationScannerNext_int
  val RelationScannerNext_double = RelationScannerIRs.RelationScannerNext_double
  type RelationScannerNext_double = RelationScannerIRs.RelationScannerNext_double
  val RelationScannerNext_char = RelationScannerIRs.RelationScannerNext_char
  type RelationScannerNext_char = RelationScannerIRs.RelationScannerNext_char
  val RelationScannerNext1 = RelationScannerIRs.RelationScannerNext1
  type RelationScannerNext1 = RelationScannerIRs.RelationScannerNext1
  val RelationScannerNext2 = RelationScannerIRs.RelationScannerNext2
  type RelationScannerNext2 = RelationScannerIRs.RelationScannerNext2
  val RelationScannerNext_string = RelationScannerIRs.RelationScannerNext_string
  type RelationScannerNext_string = RelationScannerIRs.RelationScannerNext_string
  val RelationScannerNext_date = RelationScannerIRs.RelationScannerNext_date
  type RelationScannerNext_date = RelationScannerIRs.RelationScannerNext_date
  val RelationScannerHasNext = RelationScannerIRs.RelationScannerHasNext
  type RelationScannerHasNext = RelationScannerIRs.RelationScannerHasNext
  val RelationScanner_Field_Delimiter = RelationScannerIRs.RelationScanner_Field_Delimiter
  type RelationScanner_Field_Delimiter = RelationScannerIRs.RelationScanner_Field_Delimiter
  val RelationScanner_Field_Filename = RelationScannerIRs.RelationScanner_Field_Filename
  type RelationScanner_Field_Filename = RelationScannerIRs.RelationScanner_Field_Filename
  val RelationScannerGetNumLinesInFileObject = RelationScannerIRs.RelationScannerGetNumLinesInFileObject
  type RelationScannerGetNumLinesInFileObject = RelationScannerIRs.RelationScannerGetNumLinesInFileObject
  // method definitions
   def relationScannerNew(filename : Rep[String], delimiter : Rep[Char]) : Rep[RelationScanner] = RelationScannerNew(filename, delimiter)
   def relationScannerNext_int(self : Rep[RelationScanner]) : Rep[Int] = RelationScannerNext_int(self)
   def relationScannerNext_double(self : Rep[RelationScanner]) : Rep[Double] = RelationScannerNext_double(self)
   def relationScannerNext_char(self : Rep[RelationScanner]) : Rep[Char] = RelationScannerNext_char(self)
   def relationScannerNext1(self : Rep[RelationScanner], buf : Rep[Array[Byte]]) : Rep[Int] = RelationScannerNext1(self, buf)
   def relationScannerNext2(self : Rep[RelationScanner], buf : Rep[Array[Byte]], offset : Rep[Int]) : Rep[Int] = RelationScannerNext2(self, buf, offset)
   def relationScannerNext_string(self : Rep[RelationScanner]) : Rep[String] = RelationScannerNext_string(self)
   def relationScannerNext_date(self : Rep[RelationScanner]) : Rep[Int] = RelationScannerNext_date(self)
   def relationScannerHasNext(self : Rep[RelationScanner]) : Rep[Boolean] = RelationScannerHasNext(self)
   def relationScanner_Field_Delimiter(self : Rep[RelationScanner]) : Rep[Char] = RelationScanner_Field_Delimiter(self)
   def relationScanner_Field_Filename(self : Rep[RelationScanner]) : Rep[String] = RelationScanner_Field_Filename(self)
   def relationScannerGetNumLinesInFileObject(filePath : Rep[String]) : Rep[Int] = RelationScannerGetNumLinesInFileObject(filePath)
  type RelationScanner = relation.shallow.RelationScanner
}
object RelationScannerIRs extends Base {
  // Type representation
  case object RelationScannerType extends TypeRep[RelationScanner] {
    def rebuild(newArguments: TypeRep[_]*): TypeRep[_] = RelationScannerType
    val name = "RelationScanner"
    val typeArguments = Nil
  }
      implicit val typeRelationScanner: TypeRep[RelationScanner] = RelationScannerType
  // case classes
  case class RelationScannerNew(filename : Rep[String], delimiter : Rep[Char]) extends ConstructorDef[RelationScanner](List(), "RelationScanner", List(List(filename,delimiter))){
    override def curriedConstructor = (copy _).curried
  }

  case class RelationScannerNext_int(self : Rep[RelationScanner]) extends FunctionDef[Int](Some(self), "next_int", List(List())){
    override def curriedConstructor = (copy _)
  }

  case class RelationScannerNext_double(self : Rep[RelationScanner]) extends FunctionDef[Double](Some(self), "next_double", List(List())){
    override def curriedConstructor = (copy _)
  }

  case class RelationScannerNext_char(self : Rep[RelationScanner]) extends FunctionDef[Char](Some(self), "next_char", List(List())){
    override def curriedConstructor = (copy _)
  }

  case class RelationScannerNext1(self : Rep[RelationScanner], buf : Rep[Array[Byte]]) extends FunctionDef[Int](Some(self), "next", List(List(buf))){
    override def curriedConstructor = (copy _).curried
  }

  case class RelationScannerNext2(self : Rep[RelationScanner], buf : Rep[Array[Byte]], offset : Rep[Int]) extends FunctionDef[Int](Some(self), "next", List(List(buf,offset))){
    override def curriedConstructor = (copy _).curried
  }

  case class RelationScannerNext_string(self : Rep[RelationScanner]) extends FunctionDef[String](Some(self), "next_string", List(List())){
    override def curriedConstructor = (copy _)
  }

  case class RelationScannerNext_date(self : Rep[RelationScanner]) extends FunctionDef[Int](Some(self), "next_date", List()){
    override def curriedConstructor = (copy _)
  }

  case class RelationScannerHasNext(self : Rep[RelationScanner]) extends FunctionDef[Boolean](Some(self), "hasNext", List(List())){
    override def curriedConstructor = (copy _)
  }

  case class RelationScanner_Field_Delimiter(self : Rep[RelationScanner]) extends FieldDef[Char](self, "delimiter"){
    override def curriedConstructor = (copy _)
    override def isPure = true

  }

  case class RelationScanner_Field_Filename(self : Rep[RelationScanner]) extends FieldDef[String](self, "filename"){
    override def curriedConstructor = (copy _)
    override def isPure = true

  }

  case class RelationScannerGetNumLinesInFileObject(filePath : Rep[String]) extends FunctionDef[Int](None, "RelationScanner.getNumLinesInFile", List(List(filePath))){
    override def curriedConstructor = (copy _)
  }

  type RelationScanner = relation.shallow.RelationScanner
}
trait RelationScannerImplicits extends RelationScannerOps { 
  // Add implicit conversions here!
}
trait RelationScannerComponent extends RelationScannerOps with RelationScannerImplicits {  }

trait RelationScannerPartialEvaluation extends RelationScannerComponent with BasePartialEvaluation {  
  // Immutable field inlining 
  override def relationScanner_Field_Delimiter(self : Rep[RelationScanner]) : Rep[Char] = self match {
    case Def(node: RelationScannerNew) => node.delimiter
    case _ => super.relationScanner_Field_Delimiter(self)
  }
  override def relationScanner_Field_Filename(self : Rep[RelationScanner]) : Rep[String] = self match {
    case Def(node: RelationScannerNew) => node.filename
    case _ => super.relationScanner_Field_Filename(self)
  }

  // Mutable field inlining 
  // Pure function partial evaluation
}


// QUASI GENERATED CODE:

object RelationScannerQuasiNodes extends BaseExtIR {
  import RelationScannerIRs._
  // case classes
  case class RelationScannerNewExt(filename : Rep[String], delimiter : Rep[Char]) extends FunctionDef[RelationScannerNew, RelationScanner] {
    override def nodeUnapply(t: RelationScannerNew): Option[Product] = (RelationScannerNew.unapply(t): Option[Product]) map { r =>
      r }
  }
  case class RelationScannerNext_intExt(self : Rep[RelationScanner]) extends FunctionDef[RelationScannerNext_int, Int] {
    override def nodeUnapply(t: RelationScannerNext_int): Option[Product] = (RelationScannerNext_int.unapply(t): Option[Product]) map { r =>
      r }
  }
  case class RelationScannerNext_doubleExt(self : Rep[RelationScanner]) extends FunctionDef[RelationScannerNext_double, Double] {
    override def nodeUnapply(t: RelationScannerNext_double): Option[Product] = (RelationScannerNext_double.unapply(t): Option[Product]) map { r =>
      r }
  }
  case class RelationScannerNext_charExt(self : Rep[RelationScanner]) extends FunctionDef[RelationScannerNext_char, Char] {
    override def nodeUnapply(t: RelationScannerNext_char): Option[Product] = (RelationScannerNext_char.unapply(t): Option[Product]) map { r =>
      r }
  }
  case class RelationScannerNext1Ext(self : Rep[RelationScanner], buf : Rep[Array[Byte]]) extends FunctionDef[RelationScannerNext1, Int] {
    override def nodeUnapply(t: RelationScannerNext1): Option[Product] = (RelationScannerNext1.unapply(t): Option[Product]) map { r =>
      r }
  }
  case class RelationScannerNext2Ext(self : Rep[RelationScanner], buf : Rep[Array[Byte]], offset : Rep[Int]) extends FunctionDef[RelationScannerNext2, Int] {
    override def nodeUnapply(t: RelationScannerNext2): Option[Product] = (RelationScannerNext2.unapply(t): Option[Product]) map { r =>
      r }
  }
  case class RelationScannerNext_stringExt(self : Rep[RelationScanner]) extends FunctionDef[RelationScannerNext_string, String] {
    override def nodeUnapply(t: RelationScannerNext_string): Option[Product] = (RelationScannerNext_string.unapply(t): Option[Product]) map { r =>
      r }
  }
  case class RelationScannerNext_dateExt(self : Rep[RelationScanner]) extends FunctionDef[RelationScannerNext_date, Int] {
    override def nodeUnapply(t: RelationScannerNext_date): Option[Product] = (RelationScannerNext_date.unapply(t): Option[Product]) map { r =>
      r }
  }
  case class RelationScannerHasNextExt(self : Rep[RelationScanner]) extends FunctionDef[RelationScannerHasNext, Boolean] {
    override def nodeUnapply(t: RelationScannerHasNext): Option[Product] = (RelationScannerHasNext.unapply(t): Option[Product]) map { r =>
      r }
  }
  case class RelationScanner_Field_DelimiterExt(self : Rep[RelationScanner]) extends FunctionDef[RelationScanner_Field_Delimiter, Char] {
    override def nodeUnapply(t: RelationScanner_Field_Delimiter): Option[Product] = (RelationScanner_Field_Delimiter.unapply(t): Option[Product]) map { r =>
      r }
  }
  case class RelationScanner_Field_FilenameExt(self : Rep[RelationScanner]) extends FunctionDef[RelationScanner_Field_Filename, String] {
    override def nodeUnapply(t: RelationScanner_Field_Filename): Option[Product] = (RelationScanner_Field_Filename.unapply(t): Option[Product]) map { r =>
      r }
  }
  case class RelationScannerGetNumLinesInFileObjectExt(filePath : Rep[String]) extends FunctionDef[RelationScannerGetNumLinesInFileObject, Int] {
    override def nodeUnapply(t: RelationScannerGetNumLinesInFileObject): Option[Product] = (RelationScannerGetNumLinesInFileObject.unapply(t): Option[Product]) map { r =>
      r }
  }
  type RelationScanner = relation.shallow.RelationScanner
}

trait RelationScannerExtOps extends BaseExt {
  
  import RelationScannerQuasiNodes._
  import ch.epfl.data.sc.pardis.quasi.OverloadHackObj._
  implicit class RelationScannerRep(self : Rep[RelationScanner]) {
     def next_int() : Rep[Int] = relationScannerNext_int(self)
     def next_double() : Rep[Double] = relationScannerNext_double(self)
     def next_char() : Rep[Char] = relationScannerNext_char(self)
     def next(buf : Rep[Array[Byte]])(implicit overload1 : Overloaded1) : Rep[Int] = relationScannerNext1(self, buf)
     def next(buf : Rep[Array[Byte]], offset : Rep[Int])(implicit overload2 : Overloaded2) : Rep[Int] = relationScannerNext2(self, buf, offset)
     def next_string() : Rep[String] = relationScannerNext_string(self)
     def next_date : Rep[Int] = relationScannerNext_date(self)
     def hasNext() : Rep[Boolean] = relationScannerHasNext(self)
     def delimiter : Rep[Char] = relationScanner_Field_Delimiter(self)
     def filename : Rep[String] = relationScanner_Field_Filename(self)
  }
  object RelationScanner {
     def getNumLinesInFile(filePath : Rep[String]) : Rep[Int] = relationScannerGetNumLinesInFileObject(filePath)
  }
  // constructors
   def __newRelationScanner(filename : Rep[String], delimiter : Rep[Char]) : Rep[RelationScanner] = relationScannerNew(filename, delimiter)
  
  // method definitions
   def relationScannerNew(filename : Rep[String], delimiter : Rep[Char]) : Rep[RelationScanner] = RelationScannerNewExt(filename, delimiter)
   def relationScannerNext_int(self : Rep[RelationScanner]) : Rep[Int] = RelationScannerNext_intExt(self)
   def relationScannerNext_double(self : Rep[RelationScanner]) : Rep[Double] = RelationScannerNext_doubleExt(self)
   def relationScannerNext_char(self : Rep[RelationScanner]) : Rep[Char] = RelationScannerNext_charExt(self)
   def relationScannerNext1(self : Rep[RelationScanner], buf : Rep[Array[Byte]]) : Rep[Int] = RelationScannerNext1Ext(self, buf)
   def relationScannerNext2(self : Rep[RelationScanner], buf : Rep[Array[Byte]], offset : Rep[Int]) : Rep[Int] = RelationScannerNext2Ext(self, buf, offset)
   def relationScannerNext_string(self : Rep[RelationScanner]) : Rep[String] = RelationScannerNext_stringExt(self)
   def relationScannerNext_date(self : Rep[RelationScanner]) : Rep[Int] = RelationScannerNext_dateExt(self)
   def relationScannerHasNext(self : Rep[RelationScanner]) : Rep[Boolean] = RelationScannerHasNextExt(self)
   def relationScanner_Field_Delimiter(self : Rep[RelationScanner]) : Rep[Char] = RelationScanner_Field_DelimiterExt(self)
   def relationScanner_Field_Filename(self : Rep[RelationScanner]) : Rep[String] = RelationScanner_Field_FilenameExt(self)
   def relationScannerGetNumLinesInFileObject(filePath : Rep[String]) : Rep[Int] = RelationScannerGetNumLinesInFileObjectExt(filePath)
  type RelationScanner = relation.shallow.RelationScanner
}


