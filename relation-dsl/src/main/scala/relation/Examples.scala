package relation

object Examples extends App {
  
  import RelationDSL.Predef._
  import RelationDSL.Quasicodes._

  def pgrm0 = code{
    val schema = Schema("number", "digit")
    val En = Relation.scan("data/En.csv", schema, "|")
    val selEn = En.select(x => x.getField(schema, "number") == "one")
    selEn.print
  }
  
  def pgrmA = code{    val schema = Schema("number", "digit")
    val En = Relation.scan("data/En.csv", schema, "|")
    val selEn = En.select(x => x.getField(schema, "number") == "one")
    val projEn = selEn.project(Schema("digit", "number"))
    projEn.print
  }

  def pgrmB = code{
    val EnSchema = Schema("number", "digit")
    val En = Relation.scan("data/En.csv", EnSchema, "|")
    val FrSchema = Schema("digit", "nombre")
    val Fr = Relation.scan("data/Fr.csv", FrSchema, "|")
    val EnFr = En.join(Fr, "digit", "digit")
    EnFr.print
  }

  def pgrmC = code{    val EnSchema = Schema("number", "digit")
    val En = Relation.scan("data/En.csv", EnSchema, "|")
    val FrSchema = Schema("digitf", "nombre")
    val Fr = Relation.scan("data/Fr.csv", FrSchema, "|")
    val EnFr = En.join(Fr, "digit", "digitf")
    EnFr.project(Schema("digit", "number", "nombre")).print
  }

  def pgrmD = code{
    val schema = Schema("number", "digit", "nombre")
    val En = Relation.scan("data/EnFr.csv", schema, "|")
    val selEn = En.select(x => x.getField(schema, "number") == "one")
    val projEn = selEn.project(Schema("digit", "number"))
    projEn.print
  }

  
  def pipeline(pgrm: ClosedCode[Any]) = (pgrm
    transformWith RelationLowering
    transformWith SchemaSpecialization
    transformWith ListFusion
    transformWith RowLayout
    transformWith ListToArrayBuffer
    transformWith HashMapToOpenHashMap
    transformWith OpenHashMapLowering
    transformWith ArrayBufferColumnar
  )
  
  // write results to gen-output:
  List(pgrmA,pgrmB,pgrmC,pgrmD).map(pipeline).zipWithIndex.foreach { case (resPgrm,i) =>
    //System.err.println(RelationDSL.showRep(resPgrm.rep))
    val pgrmStr = RelationDSL.showRep(resPgrm.rep) + "\n"
    import java.nio.file.{Paths, Files}
    import java.nio.charset.StandardCharsets
    Files.write(Paths.get(s"gen-output/pgrm${'A'+i toChar}.scala"), pgrmStr.getBytes(StandardCharsets.UTF_8))
  }
  
}
