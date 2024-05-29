package alpine
package codegen

import alpine.ast
import alpine.symbols
import alpine.symbols.Entity.builtinModule
import alpine.util.FatalError

import scala.annotation.tailrec
import scala.collection.mutable
import alpine.symbols.Type
import alpine.symbols.Type.Bool
import alpine.ast.Typecast
import alpine.ast.ErrorTree
import alpine.ast.Binding
import alpine.ast.ValuePattern
import alpine.ast.RecordPattern
import alpine.ast.Wildcard
import alpine.symbols.Type.Labeled
import javax.xml.stream.events.EntityReference
import alpine.ast.Identifier
// import scala_rt.rt

/** The transpilation of an Alpine program to Scala. */
final class CPrinter(syntax: TypedProgram) extends ast.TreeVisitor[CPrinter.Context, Unit]:

  import CPrinter.Context

  /** The program being evaluated. */
  private given TypedProgram = syntax

  /** Returns a Scala program equivalent to `syntax`. */
  def transpile(): String =
    given c: Context = Context()
    val output = StringBuilder()
    c.indentation += 1
    syntax.declarations.foreach(_.visit(this)) // all declarations emitted implicitely in c
    c.indentation -= 1

    output ++= "#include <stdio.h>\n#include <stdint.h>\n\n"
    c.typesToEmit.foreach(t => output ++= emitForwardDeclRecord(t))
    c.typesToEmit.foreach(t => output ++= emitRecord(t))
    c.functionsToEmit.foreach(f => output ++= emitFunction(f))
    
    output ++= "int main(void) {\n"
    output ++= c.output.toString()
    c.output ++= "return 0;\n"
    output ++= "}\n"

    output.toString

  private def emitForwardDeclRecord(t: symbols.Type.Record)(using context: Context): String =
    "typedef struct " + transpiledType(t) + " " + transpiledType(t) + ";\n"

  /** Writes the C declaration of `t` in `context`. */
  private def emitRecord(t: symbols.Type.Record)(using context: Context): String =
    val output = StringBuilder()

    output ++= "typedef struct " + transpiledType(t) + " {\n"
    for (labeled, i) <- t.fields.zipWithIndex do
      output ++= transpiledType(labeled.value) + " " + transpiledFieldName(labeled, i) + ";\n"

    output ++= ("} " + transpiledType(t) + ";\n\n")
    output.toString()

  /** Writes the C declaration of `t` in `context`. */
  private def emitFunction(t: ast.Function)(using context: Context): String =
    val output = StringBuilder()
    t.output match
      case Some(out) => output ++= transpiledType(out.tpe)
      case None =>

    output ++= " " + transpiledReferenceTo(t.entityDeclared) + "("    
    output.appendCommaSeparated(t.inputs) { (o, a) => o ++= a.identifier }
    output ++= ") {\n"
    output ++= "return "

    given c: Context = Context()
    t.body.visit(this)

    output ++= c.output.toString()
    output ++= ";\n"
    output ++= "}\n"
    output.toString()

  /** Writes the Scala declaration of `t`, which is not a singleton, in `context`. */
  private def emitNonSingletonRecord(t: symbols.Type.Record)(using context: Context): Unit =
 
      ??? 

  private def transpiledFieldName(l: Labeled, i: Int)(using context: Context): String =
    l.label.getOrElse(discriminator(l.value) + i.toString())

  /** Returns the transpiled form of `t`. */
  private def transpiledType(t: symbols.Type)(using context: Context): String =
    t match
      case u: symbols.Type.Builtin =>
        transpiledBuiltin(u)
      case u: symbols.Type.Record =>
        transpiledRecord(u)
      case u: symbols.Type.Arrow =>
        transpiledArrow(u)
      case u: symbols.Type.Sum =>
        transpiledSum(u)
      case _ => throw Error(s"type '${t}' is not representable in Scala")

  /** Returns the transpiled form of `t`. */
  private def transpiledBuiltin(t: symbols.Type.Builtin)(using context: Context): String =
    t match
      case symbols.Type.BuiltinModule => throw Error(s"type '${t}' is not representable in Scala")
      case symbols.Type.Bool => "char"
      case symbols.Type.Int => "int32_t"
      case symbols.Type.Float => "float"
      case symbols.Type.String => "char *"
      case symbols.Type.Any => "uint64_t"

  /** Returns the transpiled form of `t`. */
  private def transpiledRecord(t: symbols.Type.Record)(using context: Context): String =
    // TODO
    if t == symbols.Type.Unit then
      "Unit"
    else
      val d = discriminator(t)
      if t.fields.isEmpty then s"${d}.type" else d

  /** Returns the transpiled form of `t`. */
  private def transpiledArrow(t: symbols.Type.Arrow)(using context: Context): String =
    ???
  /** Returns the transpiled form of `t`. */
  private def transpiledSum(t: symbols.Type.Sum)(using context: Context): String =
    transpiledType(t.members.head)

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type): String =
    t match
      case u: symbols.Type.Builtin =>
        discriminator(u)
      case u: symbols.Type.Meta =>
        s"M${discriminator(u.instance)}"
      case u: symbols.Type.Definition =>
        "D" + u.identifier
      case u: symbols.Type.Record =>
        discriminator(u)
      case u: symbols.Type.Arrow =>
        discriminator(u)
      case u: symbols.Type.Sum =>
        discriminator(u)
      case _ =>
        throw Error(s"unexpected type '${t}'")

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Builtin): String =
    t match
      case symbols.Type.BuiltinModule => "Z"
      case symbols.Type.Bool => "B"
      case symbols.Type.Int => "I"
      case symbols.Type.Float => "F"
      case symbols.Type.String => "S"
      case symbols.Type.Any => "A"

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Record): String =
    val b = StringBuilder("R")
    b ++= t.identifier.substring(1) // Remove "#""
    for i <- t.fields do
      b ++= i.label.getOrElse("")
      b ++= discriminator(i.value)
    b.toString

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Arrow): String =
    val b = StringBuilder("X")
    for i <- t.inputs do
      b ++= i.label.getOrElse("")
      b ++= discriminator(i.value)
    b ++= discriminator(t.output)
    b.toString

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Sum): String =
    if t.members.isEmpty then "N" else
      "E" + t.members.map(discriminator).mkString

  /** Returns a transpiled reference to `e`. */
  private def transpiledReferenceTo(e: symbols.Entity): String =
    e match
      case symbols.Entity.Builtin(n, _) => n.identifier match
        case "print" => "printf"
        case "ineg" => "-"
        case "ilt"  => "<"
        case "ile"  => "<="
        case "igt"  => ">"
        case "ige"  => ">="
        case "lnot" => "~"
        case "iadd" => "+"
        case "isub" => "-"
        case "imul" => "*"
        case "idiv" => "/"
        // TODO : other builtins to support ?
        case _@b =>  b
      case symbols.Entity.Declaration(n, t) => scalaized(n) + discriminator(t)
      case _: symbols.Entity.Field => ???

  /** Returns a string representation of `n` suitable for use as a Scala identifier. */
  private def scalaized(n: symbols.Name): String =
    n.qualification match
      case Some(q) =>
        s"${scalaized(q)}_${n.identifier}"
      case None =>
        "_" + n.identifier

  override def visitLabeled[T <: ast.Tree](n: ast.Labeled[T])(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitBinding(n: ast.Binding)(using context: Context): Unit =
    def alpineTypeToCType(tpe: symbols.Type): Option[String] = tpe match
      case arrow:symbols.Type.Arrow => alpineTypeToCType(arrow.output)
      /* TODO : probably let transpiledType() do the job */
      /*case symbols.Type.String => Some("char *")
      case symbols.Type.Float => Some("float")
      case symbols.Type.Int | Type.Bool => Some("int32_t") */
      case symbols.Type.Record(_, _) => 
        if tpe == symbols.Type.Unit then None else Some(transpiledType(tpe)) 
      case _ => Some(transpiledType(tpe))

    val tpe = (n.ascription match
      case Some(ascription) => alpineTypeToCType(ascription.tpe)
      case None => None
    )

    val initTpe = (n.initializer match
      case Some(expr) => alpineTypeToCType(expr.tpe)
      case None => None
    )

    tpe match {
      case Some(strType) => context.output ++= strType
      case None => initTpe match
        case Some(strType) => context.output ++= strType
        case None =>
    }
    context.output ++= " "

    if (!tpe.isEmpty || !initTpe.isEmpty) then
      context.output ++= transpiledReferenceTo(n.entityDeclared)

    n.initializer match
      case Some(expr) => 
        if (!tpe.isEmpty || !initTpe.isEmpty) then
          context.output ++= " = "
        expr.visit(this)
      case None =>
    
    context.output ++= ";\n"

  override def visitTypeDeclaration(n: ast.TypeDeclaration)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitFunction(n: ast.Function)(using context: Context): Unit =
    context.registerUse(n)

  override def visitParameter(n: ast.Parameter)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitIdentifier(n: ast.Identifier)(using context: Context): Unit =
    context.output ++= transpiledReferenceTo(n.referredEntity.get.entity)

  override def visitBooleanLiteral(n: ast.BooleanLiteral)(using context: Context): Unit =
    context.output ++= n.value

  override def visitIntegerLiteral(n: ast.IntegerLiteral)(using context: Context): Unit =
    context.output ++= n.value

  override def visitFloatLiteral(n: ast.FloatLiteral)(using context: Context): Unit =
    context.output ++= n.value

  override def visitStringLiteral(n: ast.StringLiteral)(using context: Context): Unit =
    context.output ++= n.value

  override def visitRecord(n: ast.Record)(using context: Context): Unit =
    val record = n.tpe.asInstanceOf[symbols.Type.Record]
    context.registerUse(record)

    context.output ++= "{ " 
    record.fields.zipWithIndex.foreach { (a, i) => 
      context.output ++= ("." + transpiledFieldName(a, i) + " = ") 
      n.fields(i).value.visit(this)
      context.output ++= ", "
    } 
    context.output ++= " };\n"

  override def visitSelection(n: ast.Selection)(using context: Context): Unit =
    // TODO
    n.qualification.visit(this)
    n.selectee.referredEntity match
      case Some(symbols.EntityReference(e: symbols.Entity.Field, _)) =>
        context.output ++= "." + transpiledFieldName(e.whole.fields(e.index), e.index)
      case _ =>
        unexpectedVisit(n.selectee)
    

  override def visitApplication(n: ast.Application)(using context: Context): Unit =
    def emitFormatString(t: Type): String = t match
      case symbols.Type.String => "\"%s\\n\""
      case symbols.Type.Float => "\"%f\\n\""
      case symbols.Type.Int | Type.Bool => "\"%d\\n\""
      case _ => "\"\""

    n.function.visit(this)
    context.output ++= "("
    n.function.referredEntity.get.entity match
      case symbols.Entity.Builtin(e, _) => e.identifier match
        case "print" =>  
          assert(n.arguments.length == 1)
          context.output ++= emitFormatString(n.arguments(0).value.tpe) + ", "
          context.output.appendCommaSeparated(n.arguments) { (o, a) => a.value.visit(this) }
        case _ =>
          context.output.appendCommaSeparated(n.arguments) { (o, a) => a.value.visit(this) }
      case _ =>
        context.output.appendCommaSeparated(n.arguments) { (o, a) => a.value.visit(this) }
    
    context.output ++= ")\n"

  override def visitPrefixApplication(n: ast.PrefixApplication)(using context: Context): Unit =
    n.function.visit(this)
    //context.output ++= "("
    n.argument.visit(this)
    //context.output ++= ")"

  override def visitInfixApplication(n: ast.InfixApplication)(using context: Context): Unit =
    context.output ++= "("
    n.lhs.visit(this)
    context.output ++= " "
    n.function.visit(this)
    context.output ++= " "
    n.rhs.visit(this)
    context.output ++= ")"

  override def visitConditional(n: ast.Conditional)(using context: Context): Unit =
    context.output ++= "if ("
    n.condition.visit(this)
    context.output ++= ") {\n"
    n.successCase.visit(this)
    context.output ++= ";\n"
    context.output ++= "} else {\n"
    n.failureCase.visit(this)
    context.output ++= ";\n"
    context.output ++= "}\n"

  override def visitMatch(n: ast.Match)(using context: Context): Unit =
    context.output ++= "switch ("
    n.scrutinee.visit(this)
    context.output ++= ") {\n"
    n.cases.foreach(_.visit(this))
    context.output ++= "}\n"

  override def visitMatchCase(n: ast.Match.Case)(using context: Context): Unit =
    context.output ++= "case "
    n.pattern.visit(this)
    context.output ++= ": "
    n.body.visit(this)
    context.output ++= ";\n"
    context.output ++= "break;\n"

  override def visitLet(n: ast.Let)(using context: Context): Unit =
    ???

  override def visitLambda(n: ast.Lambda)(using context: Context): Unit =
    ???

  override def visitParenthesizedExpression(
      n: ast.ParenthesizedExpression
  )(using context: Context): Unit =
    context.output ++= "("
    n.inner.visit(this) 
    context.output ++= ")"

  override def visitAscribedExpression(
      n: ast.AscribedExpression
  )(using context: Context): Unit =
    n.operation match
      case Typecast.NarrowUnconditionally | Typecast.Widen =>
        context.output ++= "(" + transpiledType(n.ascription.tpe) + ") "
        n.inner.visit(this)
      case Typecast.Narrow => unexpectedVisit(n) 
    

  override def visitTypeIdentifier(n: ast.TypeIdentifier)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitRecordType(n: ast.RecordType)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitTypeApplication(n: ast.TypeApplication)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitArrow(n: ast.Arrow)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitSum(n: ast.Sum)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitParenthesizedType(n: ast.ParenthesizedType)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitValuePattern(n: ast.ValuePattern)(using context: Context): Unit =
    n.value.visit(this)

  override def visitRecordPattern(n: ast.RecordPattern)(using context: Context): Unit =
    ???

  override def visitWildcard(n: ast.Wildcard)(using context: Context): Unit =
    ???

  override def visitError(n: ast.ErrorTree)(using context: Context): Unit =
    unexpectedVisit(n)

object CPrinter:

  /** The local state of a transpilation to Scala.
   *
   *  @param indentation The current identation to add before newlines.
   */
  final class Context(var indentation: Int = 0):

    /** The types that must be emitted in the program. */
    private var _typesToEmit = mutable.Set[symbols.Type.Record]()
    private var _functionsToEmit = mutable.Set[ast.Function]()

    /** The types that must be emitted in the program. */
    def typesToEmit: Set[symbols.Type.Record] = _typesToEmit.toSet

    def functionsToEmit: Set[ast.Function] = _functionsToEmit.toSet 

    /** The (partial) result of the transpilation. */
    private var _output = StringBuilder()

    /** The (partial) result of the transpilation. */
    def output: StringBuilder = _output

    /** `true` iff the transpiler is processing top-level symbols. */
    private var _isTopLevel = true

    /** `true` iff the transpiler is processing top-level symbols. */
    def isTopLevel: Boolean = _isTopLevel

    /** Adds `t` to the set of types that are used by the transpiled program. */
    def registerUse(t: symbols.Type.Record): Unit =
      if t != symbols.Type.Unit then _typesToEmit.add(t)

    def registerUse(t: ast.Function): Unit =
      _functionsToEmit.add(t)

    /** Returns `action` applied on `this` where `output` has been exchanged with `o`. */
    def swappingOutputBuffer[R](o: StringBuilder)(action: Context => R): R =
      val old = _output
      _output = o
      try action(this) finally _output = old

    /** Returns `action` applied on `this` where `isTopLevel` is `false`. */
    def inScope[R](action: Context => R): R =
      var tl = _isTopLevel
      _isTopLevel = false
      try action(this) finally _isTopLevel = tl

  end Context

end CPrinter