package alpine
package codegen

import alpine.symbols
import alpine.wasm.WasmTree._
import alpine.ast._
import alpine.wasm.Wasm
import scala.collection.mutable
import scala.collection.immutable.HashMap
import scala.math.Numeric.BigDecimalAsIfIntegral
import alpine.symbols.Type

/** The transpilation of an Alpine program to Scala. */
final class CodeGenerator(syntax: TypedProgram) extends ast.TreeVisitor[CodeGenerator.Context, Unit]:
  import CodeGenerator._

  /** The program being evaluated. */
  private given TypedProgram = syntax

  given c: Context = Context()
    syntax.declarations.foreach(_.visit(this))

  /** Returns a WebAssembly program equivalent to `syntax`. */
  /** THIS IS AN EXAMPLE MODULE! */
  def compile(): Module = Module(
    List(
      ImportFromModule("api", "print", "print", List(I32), None),
      ImportFromModule("api", "print", "fprint", List(F32), None),
      ImportFromModule("api", "print-char", "print-char", List(I32), None),
      ImportFromModule("api", "show-memory", "show-memory", List(I32), None),
      ImportMemory("api", "mem", 100)
    ),
    //List(
      /*FunctionDefinition("heap-test", body =
        List(
          IConst(0),
          IConst(0xdeadbeef),
          IStore,
          IConst(0),
          Call("show-memory")
        )
      ),
      FunctionDefinition("local-test", locals = List(F32, F32), returnType = Some(F32), body =
        List(
          FConst(3.14),
          LocalSet(0),
          FConst(1.67),
          LocalSet(1),
          LocalGet(0),
          LocalGet(1),
          FSub
        )
      ),*/
    c.generateFunctionList().appended(c.generateMain())
  )

  // Utilitary functions

  private def typeSize(t: symbols.Type): Int = t match
    case alpine.symbols.Type.Int => 4
    case alpine.symbols.Type.Float => 4
    case _ => 4

  // Tree visitor methods

  /** Visits `n` with state `a`. */
  def visitLabeled[T <: Tree](n: Labeled[T])(using a: Context): Unit = 
    n.value.visit(this) // note sure and definitely not complete

  /** Visits `n` with state `a`. */
  def visitBinding(n: Binding)(using a: Context): Unit = 
    /* Create stack space */
    n.ascription match // for now, we only support Int and Float
      case Some(t) => t.tpe match
        case alpine.symbols.Type.Int => //a.pushLocal(n.identifier, I32)
        case alpine.symbols.Type.Float => //a.pushLocal(n.identifier, F32)
        case _ => // Do nothing 
      case None => // Do nothing

    /* Evaluate the rhs of the binding */
    n.initializer match
      case Some(e) => 
        e.visit(this)
      /* Assign local value */
        e.tpe match 
          case alpine.symbols.Type.Unit =>
          case alpine.symbols.Type.Int | alpine.symbols.Type.Float | alpine.symbols.Type.Record(_, _) =>
            println(e.tpe)
            val tpe = I32 // TODO, do something about it with e.tpe
            a.pushLocal(n.identifier, tpe)
            a.addInstruction(LocalSet(a.getLocal(n.identifier).get.position))
            println("push a local")
          case _ => 
        e.tpe match
          case r@alpine.symbols.Type.Record(_, _) => a.registerRecord(n.identifier, r)
          case _ =>
      case _ => 


  /** Visits `n` with state `a`. */
  def visitTypeDeclaration(n: TypeDeclaration)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitFunction(n: ast.Function)(using a: Context): Unit = {
    /* TODO : visit the body recursively and register all bindings and lets
     * through the context state as locals. We refer to those locals on the fly while
     * visiting the body recusrively.
     */

    n.body.visit(this)

    /* TODO : if everything has been done right, we can get through the context the set of
      * locals registered so far, and create our final FunctionDefinition */

    val name = n.identifier
    val params = n.inputs.map(_.ascription match
      case Some(value) => value.tpe match
        case alpine.symbols.Type.Float => F32
        case alpine.symbols.Type.Int => I32
        case _ => I32 // FIXME : à défaut...
      case None => I32 // FIXME : à défaut...
    )
    val locals = a.allLocals().sortBy(_.position).map(_.tpe)
    val returnType = n.output.map(_.tpe match
      case alpine.symbols.Type.Float => F32
      case alpine.symbols.Type.Int => I32
      case _ => I32
    ) 
    val body = a.funcInstructions.toList
    a.clearFuncInstructions()
    a.addFunctionDefinition(FunctionDefinition(name, params, locals, returnType, body))
    a.clearLocals() // We exit the context of this function, clear locals.
  }

  /** Visits `n` with state `a`. */
  def visitParameter(n: Parameter)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitIdentifier(n: Identifier)(using a: Context): Unit = 
    println(a.allLocals())
    a.addInstruction(LocalGet(a.getLocal(n.value).get.position))

  /** Visits `n` with state `a`. */
  def visitBooleanLiteral(n: BooleanLiteral)(using a: Context): Unit = 
    a.addInstruction(IConst(if n.value == "true" then 1 else 0)) // Pushes this constant to the stack, not a local !

  /** Visits `n` with state `a`. */
  def visitIntegerLiteral(n: IntegerLiteral)(using a: Context): Unit = 
    a.addInstruction(IConst(n.value.toInt)) // Pushes this constant to the stack, not a local !

  /** Visits `n` with state `a`. */
  def visitFloatLiteral(n: FloatLiteral)(using a: Context): Unit = 
    a.addInstruction(FConst(n.value.toFloat)) // Pushes this constant to the stack, not a local !

  /** Visits `n` with state `a`. */
  def visitStringLiteral(n: StringLiteral)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitRecord(n: Record)(using a: Context): Unit =
    /* Allocate some memory for the record, and register it somewhere ? */
    val fieldsType = n.fields.map(_.value.tpe)
    val recordSize = fieldsType.map(typeSize(_)).sum

    val recordAddress = a.allocateRecord(recordSize)
    var offset = 0

    /* Visit the record declaration. Note that visiting the fields will produce instructions that push all values on the stack.
     * Then, we just need to collect everything that's on the stack and store in memory */
    n.fields.foreach (f =>
      /* Put memory index */
      a.addInstruction(IConst(recordAddress + offset))
      offset += typeSize(f.value.tpe)

      /* Visit the field, which ultimately results in adding an instruction to put the value on the stack */
      f.value.visit(this)

      f.value.tpe match
        case alpine.symbols.Type.Int => 
          a.addInstruction(IStore)
        case alpine.symbols.Type.Float => 
          a.addInstruction(FStore)
        case _ =>
          a.addInstruction(IStore)
    )

    /* Pushes the resulting record address on the stack */
    a.addInstruction(IConst(recordAddress))


  /** Visits `n` with state `a`. */
  def visitSelection(n: Selection)(using a: Context): Unit =
    /* Visiting the qualification should give us the identifier of the entity we want to access on the stack */
    // TODO : we need to find out which record we are referring to
    //val qualificationID = n.qualification.referredEntity.map(entity => entity.entity.name.identifier).get 
    val qual = n.qualification
    println(f"qualification expression : $qual")
    val qualificationID = n.qualification match
      case Identifier(name, _) => name
      case _ => "" // FIXME
    
    println(f"qualification: $qualificationID")
    val record = a.getRecord(qualificationID).get
    val fieldsSize = record.fields.map(f => typeSize(f.value))
    
    n.qualification.visit(this)

    val (fieldOffset, fieldType) = n.selectee match
      case Identifier(value, _) =>
        val until = record.fields.indexWhere(_.label.get == value)
        (fieldsSize.slice(0, until).sum, record.fields.find(_.label.get == value).get.value)
      case IntegerLiteral(value, _) => 
        (fieldsSize.slice(0, value.toInt).sum, record.fields(value.toInt).value)

    /* The result of this should give us the address of the field in memory, which is then found on the stack */
    a.addInstruction(IConst(fieldOffset))
    a.addInstruction(IAdd)

    /* Push the value at specified memory on the stack */
    fieldType match
      case alpine.symbols.Type.Int =>
        a.addInstruction(ILoad)
      case alpine.symbols.Type.Float =>
        a.addInstruction(FLoad)
      case _ =>
        a.addInstruction(ILoad)

  /** Visits `n` with state `a`. */
  def visitApplication(n: Application)(using a: Context): Unit = 
    n.arguments.foreach(_.visit(this))
    if(n.function.isInstanceOf[Identifier] && n.function.asInstanceOf[Identifier].value == "print")
      n.arguments(0).value.tpe match
      case symbols.Type.Int => 
        a.addInstruction(Call("print"))
      case symbols.Type.Float => 
        a.addInstruction(Call("fprint"))
      case _ =>  // TODO : do nothing ?
    else
      n.arguments.foreach(_.value.referredEntity match
        case Some(entityRef) => 
          a.getLocal(entityRef.entity.name.identifier) match
            case Some(local) => a.addInstruction(LocalGet(local.position))
            case None => assert(false) // OUPS ! there should be a local registered
        case None => // TODO : is this a litteral in this case ?
      )
      a.addInstruction(Call(n.function.asInstanceOf[Identifier].value))  

  /** Visits `n` with state `a`. */
  def visitPrefixApplication(n: PrefixApplication)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitInfixApplication(n: InfixApplication)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitConditional(n: Conditional)(using a: Context): Unit =
    n.condition.visit(this)

    a.pushIfBlock()
    n.successCase.visit(this)
    val _then = a.popIfBlock()

    a.pushIfBlock()
    n.failureCase.visit(this)
    val _else = a.popIfBlock()

    val i = n.tpe match 
      case alpine.symbols.Type.Int => If_i32(_then, Some(_else))
      case _ => If_void(_then, Some(_else)) // Assume void

    a.addInstruction(i)

  /** Visits `n` with state `a`. */
  def visitMatch(n: Match)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitMatchCase(n: Match.Case)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitLet(n: Let)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitLambda(n: Lambda)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitParenthesizedExpression(n: ParenthesizedExpression)(using a: Context): Unit =
    n.inner.visit(this)

  /** Visits `n` with state `a`. */
  def visitAscribedExpression(n: AscribedExpression)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitTypeIdentifier(n: TypeIdentifier)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitRecordType(n: RecordType)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitTypeApplication(n: TypeApplication)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitArrow(n: Arrow)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitSum(n: Sum)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitParenthesizedType(n: ParenthesizedType)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitValuePattern(n: ValuePattern)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitRecordPattern(n: RecordPattern)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitWildcard(n: Wildcard)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitError(n: ErrorTree)(using a: Context): Unit = ???


object CodeGenerator:
  case class Local(name: String, tpe: WasmType, position: Int)

  /** The local state of a transpilation to Scala.
   *
   *  @param indentation The current identation to add before newlines.
   */
  final class Context(var indentation: Int = 0):

    /** The types that must be emitted in the program. */
    private var _typesToEmit = mutable.Set[symbols.Type.Record]()
    private var records = mutable.HashMap[String, symbols.Type.Record]()

    /** The types that must be emitted in the program. */
    def typesToEmit: Set[symbols.Type.Record] = _typesToEmit.toSet

    def registerRecord(name: String, r: alpine.symbols.Type.Record): Unit = records.addOne((name, r))
    def getRecord(name: String): Option[alpine.symbols.Type.Record] = records.get(name)

    /** The (partial) result of the transpilation. */
    private var _output = StringBuilder()

    /** The (partial) result of the transpilation. */
    def output: StringBuilder = _output

    val funcInstructions = mutable.ListBuffer[Instruction]()
    val ifBlockInstructions = mutable.ListBuffer[mutable.ListBuffer[Instruction]]()

    def addInstruction(i: Instruction): Unit = 
      if ifBlockInstructions.isEmpty then
        funcInstructions += i
      else
        addIfBlockInstruction(i)

    def clearFuncInstructions(): Unit = funcInstructions.clear()

    def addIfBlockInstruction(i: Instruction): Unit = 
      assert(!ifBlockInstructions.isEmpty)
      ifBlockInstructions.last += i

    def pushIfBlock(): Unit = ifBlockInstructions.addOne(mutable.ListBuffer[Instruction]())
    def popIfBlock(): List[Instruction] = ifBlockInstructions.remove(ifBlockInstructions.size-1).toList

    def generateMain(): MainFunction = 
      MainFunction(
        List(
          Call("topLevel")
        ),
        None
        //Some(I32) // what is the return type of the main function ?
      )

    private var localIdx = 0
    private val locals = mutable.HashMap[String, Local]()

    def pushLocal(name: String, tpe: WasmType): Unit = 
      locals.addOne(name, Local(name, tpe, localIdx))
      localIdx += 1

    def getLocal(name: String): Option[Local] =
      locals.get(name)

    def allLocals(): List[Local] = locals.toList.map(_._2)

    def clearLocals(): Unit =
      locals.clear()
      localIdx = 0

    val functions = mutable.ListBuffer[FunctionDefinition]()
    def addFunctionDefinition(f: FunctionDefinition): Unit = functions += f

    def generateFunctionList(): List[FunctionDefinition] = 
      val name = "topLevel"
      val params = Nil
      val locals = allLocals().sortBy(_.position).map(_.tpe)
      val returnType = None
      val body = funcInstructions.toList
      clearFuncInstructions()
      addFunctionDefinition(FunctionDefinition(name, params, locals, returnType, body))
      clearLocals()

      functions.toList

    /** Memory management */
    private var memoryPointer: Int = 0

    /** Allocates some space for a record of specified size. Returns the address of the start of this record */
    def allocateRecord(size: Int): Int =
      val address = memoryPointer
      memoryPointer += size
      address

    /** `true` iff the transpiler is processing top-level symbols. */
    private var _isTopLevel = true

    /** `true` iff the transpiler is processing top-level symbols. */
    def isTopLevel: Boolean = _isTopLevel

    /** Adds `t` to the set of types that are used by the transpiled program. */
    def registerUse(t: symbols.Type.Record): Unit =
      if t != symbols.Type.Unit then _typesToEmit.add(t)

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
