package alpine.codegen

import scala.io.Source

class CodegenCTests extends munit.FunSuite:
  private val lineSeparator = System.lineSeparator()

  var runner: Option[CodegenCUtils.Runner] = None
  
  val inputFileAlpineTests = "./src/test/res/codegen/test_cases.al"

  /**
    * Parses the given file and run the obtained test cases
    *
    * @param filename
    * @param loc
    */
  def runTestsFromFile(filename: String)(implicit loc: munit.Location): Unit = {
    val lines: List[String] = Source.fromFile(filename).getLines().toList
    val tests = CodegenCUtils.parseTests(lines)
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
    runner = Some(CodegenCUtils.Runner())

  override def afterAll(): Unit =
    //runner.foreach(_.delete)
    runner.foreach(_)

  runTestsFromFile(inputFileAlpineTests)