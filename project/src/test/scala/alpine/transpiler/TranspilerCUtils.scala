package alpine.transpiler

import alpine._
import sys.process._
import scala.io.Source
import java.nio.file.{ Files, Path }
import java.util.Comparator
import java.io.File
import alpine.wasm.Wasm


object TranspilerCUtils:
   /**
    * Represents a test case for the code generator WASM
    *
    * @param name The name of the test, with the number of points as (Xpts) or (Xpt)
    * @param input The lines of the input file, i.e., the alpine code
    * @param expected The expected lines of the std out when running the transpiled program
    */
  case class TranspilerCTest(name: String, input: List[String], expected: List[String])

  /**
    * Parses a file with the correct format and produces a list of TranspilerTest instances
    *
    * @param lines
    * @return
    */
  def parseTests(lines: List[String]): List[TranspilerCTest] = 
    val indicesBeginningTests = lines.zipWithIndex.filter(p => p._1.startsWith("//BEGIN")).map(_._2)
    val indicesEndTests = lines.zipWithIndex.filter(p => p._1.startsWith("//END")).map(_._2 + 1)
    val boundsOfTests = indicesBeginningTests.zip(indicesEndTests)
    val testLists = boundsOfTests.map(p => lines.slice(p._1, p._2))
    testLists.map(l =>
      val name = l.head.replace("//BEGIN ", "")
      val codeEndIndex = l.tail.indexWhere(s => s.startsWith("//OUT"))
      val code = l.tail.slice(0, codeEndIndex).filter(!_.isEmpty())
      val out = l.slice(l.indexWhere(_.startsWith("//OUT")), l.indexWhere(_.startsWith("//END"))).filter(!_.contains("//OUT"))
      println(f"name = '$name'")
      println(f"code = $code")
      println(f"out = $out")
      TranspilerCTest(name = name, input = code, expected = out)
    )

  

  /** Util class that spawns a temporary directory to run Scala files */
  class Runner:
    case class CRunError(message: String) extends Exception(message):
      override def toString: String = f"Failed to run C program: \n$message"
    case class CCompileError(message: String) extends Exception(message):
      override def toString: String = f"Failed to compile C program: \n$message"
    case class BackendError(exception: Throwable) extends Exception(exception):
      override def toString: String = f"Error from Alpine: \n$exception"

    val tmpDir = Files.createTempDirectory("codegenC")

    /** Executes the cmd and returns (errorCode, output) */
    private def spawn(cmd: String, cwd: Option[File] = Some(tmpDir.toFile), ignoreStderr: Boolean = false): (Int, String, String) =
      val output = new StringBuilder
      val stderr = new StringBuilder
      val logger = ProcessLogger(x => output.append(f"$x\n"), x => stderr.append(f"$x\n")) // Ignoring stderr
      val errorCode = Process(cmd, Some(tmpDir.toFile)).!(logger)
      (errorCode, output.toString, stderr.toString)

    def compileC(input: Path): Either[CCompileError, Path] =
      val absolutePaths = input.toAbsolutePath().toString()
      val (exitCode, output, stderr) = spawn(f"gcc $absolutePaths -o $absolutePaths.bin", ignoreStderr = false)
      // 255 (-1) is reserved for panic
      if exitCode == 0 || exitCode == 255 then Right(tmpDir.resolve(absolutePaths + ".bin"))
      else Left(CCompileError("Exit code: " ++ exitCode.toString ++ "\n" ++ output ++ "\n-- stderr --\n" ++ stderr))

    /** Runs the given C program in the temporary directory */
    def run(input: Path): Either[CRunError, String] =
      val absolutePaths = input.toAbsolutePath()
      val (exitCode, output, stderr) = spawn(f"./$absolutePaths")
      // 255 (-1) is reserved for panic
      if exitCode == 0 || exitCode == 255 then Right(output.mkString("\n"))
      else Left(CRunError("Exit code: " ++ exitCode.toString ++ "\n" ++ output.mkString("\n") ++ "\n-- stderr --\n" ++ stderr.mkString("\n")))

    /**
    * Run the alpine codegen to wasm, returning the Path to the generated .wat file
    *
    * @param inputFile
    * @return
    */
    def runAlpineCompiler(inputFile: Path): Either[BackendError, Path] =
      try
        val source = SourceFile.withContentsOfFile(inputFile.toAbsolutePath.toString).get
        val parsed = parsing.Parser(source).program()
        val typed = { val typer = typing.Typer(); typer.check(parsed) }
        val compiled = codegen.CPrinter(typed).transpile()
        val CfileName = appendCExtension(inputFile.toString())
        val Cfile = tmpDir.resolve(CfileName)
        Files.write(Cfile, compiled.getBytes)
        Right(Cfile)
      catch (e: Throwable) =>
        Left(BackendError(e))

    /** Writes an Alpine file in the temporary directory. Prepends the .al if needed extension */
    def writeAlpineFile(name: String, content: String): Path =
      val file = tmpDir.resolve(appendAlpineExtension(name))
      Files.write(file, content.getBytes)
      file

    /** Deletes the temporary directory */
    def delete: Unit =
      Files.walk(tmpDir)
        .sorted(Comparator.reverseOrder())
        .map(_.toFile)
        .forEach(_.delete)

    private def appendCExtension(filename: String): String =
      if filename.endsWith(".c") then filename else f"$filename.c"

    private def appendAlpineExtension(filename: String): String =
      if filename.endsWith(".al") then filename else f"$filename.al"
