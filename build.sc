import mill._, scalalib._

/**
  * Use
  * `mill mill.scalalib.GenIdea/idea` to regenerate Idea project
  * after making changes in this file.
  */
object advent extends SbtModule {
  def scalaVersion = "2.12.8"
  override def artifactName = "advent"
//  def mainClass = Some("org.primetalk.advent.day1")
  override def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:2.1.0",
    ivy"ru.primetalk::rewritable-tree:0.1.0",
    ivy"org.typelevel::spire::0.16.2"
  )
  object test extends Tests {
    def testFrameworks =  Seq(
      "org.scalatest.tools.Framework"
    )
    override def ivyDeps =
      Agg(
        ivy"org.scalatest::scalatest::3.0.5-M1",
        ivy"org.scalacheck::scalacheck::1.13.5",
        ivy"com.chuusai::shapeless::2.3.3"
      )
    override def sources = T.sources(
      millSourcePath / "src"/ "test"/ "scala"
    )

  }
}
