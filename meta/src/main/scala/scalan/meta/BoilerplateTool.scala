package scalan.meta

object StarterBoilerplateTool extends BoilerplateTool {
  val starterTypeSynonims = Map(
    "MyArr" -> "MyArray"
    // declare your type synonims for User Defined types here (see type PA[A] = Rep[PArray[A]])
  )
  lazy val starterConfig = CodegenConfig(
    name = "ml",
    srcPath = "src/main/scala",
    entityFiles = List(
      "scalan/examples/MyArrays.scala"
    ),
    starterTypeSynonims
  )

  override def getConfigs(args: Array[String]) = Seq(starterConfig)

  override def main(args: Array[String]) = super.main(args)
}
