package alpine.transpiler

import scala.io.Source
import alpine.transpiler.TranspilerCUtils

class TranspilerCTests extends munit.FunSuite:
  private val lineSeparator = System.lineSeparator()

  var runner: Option[TranspilerCUtils.Runner] = None
  
  val inputFileAlpineTests = "./src/test/res/transpiler/test_cases.al"

  /**
    * Parses the given file and run the obtained test cases
    *
    * @param filename
    * @param loc
    */
  def runTestsFromFile(filename: String)(implicit loc: munit.Location): Unit = {
    val lines: List[String] = Source.fromFile(filename).getLines().toList
    val tests = alpine.transpiler.TranspilerCUtils.parseTests(lines)
    for t <- tests do
      test(t.name) {
        val r = runner.get
        //val alpineTestFilename = "Input.al"
        val alpineTestFilename = t.name.filter(!_.isWhitespace) + ".al"
        val inputAlpineFilePath = r.writeAlpineFile(alpineTestFilename, t.input.mkString(lineSeparator))
        val outputCFile = r.runAlpineCompiler(inputAlpineFilePath)
        val outputOfCProgram = outputCFile.flatMap(outputCFile => r.run(outputCFile).map(_.replace("\r\n", "\n")))
        outputOfCProgram match {
          case Right(output) =>
            val expected = t.expected.mkString(lineSeparator)
            assertEquals(output.stripSuffix("\n"), expected)
          case Left(error) =>
            throw error
        }
      }
  }

  override def beforeAll(): Unit =
    runner = Some(alpine.transpiler.TranspilerCUtils.Runner())

  override def afterAll(): Unit =
    //runner.foreach(_.delete)
    runner.foreach(_)

  runTestsFromFile(inputFileAlpineTests)