package alpine
package parsing

import alpine.ast
import alpine.util.FatalError

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.SeqView.Reverse
import alpine.symbols.Type.option
import alpine.ast.Tree.walkRoots
import scala.compiletime.ops.boolean

class Parser(val source: SourceFile):

  import ast.*
  import Token.Kind as K

  /** The token stream. */
  private var tokens = Lexer(source)

  /** The boundary of the last consumed token. */
  private var lastBoundary = 0

  /** The next token in the stream if it has already been produced. */
  private var lookahead: Option[Token] = None

  /** The errors collected by the parser. */
  private var errors = mutable.ListBuffer[SyntaxError]()

  /** A stack of predicates identifying tokens suitable for error recovery. */
  private var recoveryPredicates = mutable.ListBuffer[Token => Boolean]()

  /** The diagnostics collected by the parser. */
  def diagnostics: DiagnosticSet =
    DiagnosticSet(errors)

  // --- Declarations ---------------------------------------------------------

  /** Parses and returns a program. */
  def program(): alpine.Program =
    @tailrec def loop(partialResult: List[Declaration]): IArray[Declaration] =
      if peek != None then
        loop(partialResult :+ topLevel())
      else
        IArray.from(partialResult)
    Program(loop(List()))

  /** Parses and returns a top-level declaration. */
  def topLevel(): Declaration =
    peek match
      case Some(Token(K.Let, _)) =>
        binding()
      case Some(Token(K.Fun, _)) =>
        function()
      case Some(Token(K.Type, _)) =>
        typeDeclaration()
      case _ =>
        recover(ExpectedTree("top-level declaration", emptySiteAtLastBoundary), ErrorTree.apply)

  /** Parses and returns a binding declaration. */
  private[parsing] def binding(initializerIsExpected: Boolean = true): Binding =
    val initial = expect(K.Let)
    val identifier = expect(K.Identifier)
    peek match
      case Some(Token(K.Colon, _)) =>
        take()
        val ascription = Some(tpe())

        peek match
          case Some(Token(K.Eq, _)) if initializerIsExpected =>
            take()
            val initializer = Some(expression())
            Binding(identifier.site.text.toString,ascription,initializer,identifier.site.extendedTo(lastBoundary))

          case Some(Token(K.Eq, _)) if !initializerIsExpected=>
            report(SyntaxError("initializer not expected", emptySiteAtLastBoundary))
            Binding(identifier.site.text.toString,ascription,None,identifier.site.extendedTo(lastBoundary))

          case _ if initializerIsExpected =>
            report(ExpectedTokenError(K.Eq, emptySiteAtLastBoundary))
            Binding(identifier.site.text.toString,ascription,None,identifier.site.extendedTo(lastBoundary))

          case _ =>
           Binding(identifier.site.text.toString,ascription,None,identifier.site.extendedTo(lastBoundary))

      case Some(Token(K.Eq, _)) if initializerIsExpected=>
        take()
        val initializer = Some(expression())
        Binding(identifier.site.text.toString,None,initializer,identifier.site.extendedTo(lastBoundary))

      case Some(Token(K.Eq, _)) if !initializerIsExpected=>
            report(SyntaxError("initializer not expected", emptySiteAtLastBoundary))
            Binding(identifier.site.text.toString,None,None,identifier.site.extendedTo(lastBoundary))

      case _ if initializerIsExpected =>
        report(ExpectedTokenError(K.Eq, emptySiteAtLastBoundary))
        Binding(identifier.site.text.toString,None,None,identifier.site.extendedTo(lastBoundary))

      case _ =>
        Binding(identifier.site.text.toString,None,None,identifier.site.extendedTo(lastBoundary))


  /** Parses and returns a function declaration. */
  private[parsing] def function(): Function =
    val funToken = expect(K.Fun)

    val backup = snapshot()
    // if the token after the next token is a dot, then it's a method, and we need to parse the type
    take()
    peek match
      case Some(Token(K.Dot, _)) =>
        // it's a method
        restore(backup)
        val t = tpe()
        expect(K.Dot)
        val funId = functionIdentifier()
        val parameterList = valueParameterList() :+ Parameter(None, "self", Some(t), t.site.extendedTo(lastBoundary))

        val retType = peek match
          case Some(Token(K.Arrow, _)) =>
            take(); Some(tpe())
          case Some(_) | None => None

        expect(K.LBrace)
        val body = expression()
        expect(K.RBrace)

        Function(funId, List.empty, parameterList, retType, body, funToken.site)

      case _ => 
        restore(backup)
        val funId = functionIdentifier()
        val parameterList = valueParameterList()

        val retType = peek match
          case Some(Token(K.Arrow, _)) =>
            take(); Some(tpe())
          case Some(_) | None => None

        expect(K.LBrace)
        val body = expression()
        expect(K.RBrace)

        Function(funId, List.empty, parameterList, retType, body, funToken.site)

  /** Parses and returns the identifier of a function. */
  private def functionIdentifier(): String =
    take(K.Identifier) match
      case Some(s) =>
        s.site.text.toString
      case _ if peek.map((t) => t.kind.isOperatorPart).getOrElse(false) =>
        operatorIdentifier()(1).text.toString
      case _ =>
        missingName

  /** Parses and returns a list of parameter declarations in parentheses. */
  private[parsing] def valueParameterList(): List[Parameter] =
    inParentheses(() => commaSeparatedList(K.RParen.matches, parameter))
      .collect({ case p: Parameter => p })

  /*private[parsing] def parameterLabelHelper(): Option[String] =
    peek match
      case Some(Token(K.Identifier, _)) =>
        val s = take(K.Identifier)
        Some(s.site.text.toString)
      case Some(Token(t,_)) if t.isKeyword =>
        val s = take()
        Some(s.get.site.text.toString)
      case Some(Token(K.Underscore,_)) =>
        take()
        None
      case _ =>
         Some(missingName) // not sure, since that means we will have to use "_" as the label when no label is found
         */

  /** Parses and returns a parameter declaration. */
  private[parsing] def parameter(): Declaration =
    val (label: Option[String], id: Token) = peek match
      case Some(Token(K.Identifier, _)) =>
        val tempID = expect(K.Identifier)
        peek match
          case Some(Token(K.Identifier, _)) =>
            (Some(tempID.site.text.toString), expect(K.Identifier))
          case _ => 
            (None, tempID)
      case Some(Token(t,_)) if t.isKeyword =>
        val s = take()
        (Some(s.get.site.text.toString), expect(K.Identifier))
      case Some(Token(K.Underscore,_)) =>
        take()
        (None, expect(K.Identifier))
      case _ =>
        (None, expect(K.Identifier))
    
    peek match
      case Some(Token(K.Colon, _)) =>
        take()
        val t = tpe()
        Parameter(label,id.site.text.toString,Some(t),id.site.extendedTo(lastBoundary))
      case _ =>
        Parameter(label,id.site.text.toString,None,id.site.extendedTo(lastBoundary))


  /** Parses and returns a type declaration. */
  private[parsing] def typeDeclaration(): TypeDeclaration =
    val t = expect(K.Type)
    val id = expect(K.Identifier)
    expect(K.Eq)
    val body = tpe()
    TypeDeclaration(id.site.text.toString,Nil,body,t.site.extendedTo(lastBoundary))

  /** Parses and returns a list of parameter declarations in angle brackets. */
  //--- This is intentionally left in the handout /*+++ +++*/
  private def typeParameterList(): List[Parameter] =
    inAngles(() => commaSeparatedList(K.RAngle.matches, parameter))
      .collect({ case p: Parameter => p })

  // --- Expressions ----------------------------------------------------------

  /** Parses and returns a term-level expression. */
  def expression(): Expression =
    infixExpression()

  /** Parses and returns an infix expression. */
  private[parsing] def infixExpression(precedence: Int = ast.OperatorPrecedence.min): Expression = infixExpressionHelper(ascribed(), precedence)

  /** pseudocode of the algorithm : 
  lookahead := peek next token
  while lookahead is a binary operator whose precedence is >= min_precedence
      op := lookahead
      advance to next token
      rhs := parse_primary ()
      lookahead := peek next token
      while lookahead is a binary operator whose precedence is greater
                than op's, or a right-associative operator
                whose precedence is equal to op's
          rhs := parse_expression_1 (rhs, precedence of op + (1 if lookahead precedence is greater, else 0))
          lookahead := peek next token
      lhs := the result of applying op with operands lhs and rhs
  return lhs */
  private[parsing] def infixExpressionHelper(lhsGiven : Expression, minPrecedence: Int = ast.OperatorPrecedence.min): Expression =
    def loop1(lhs: Expression): Expression =
      peek match
        case Some(t) =>
          if (!t.kind.isOperatorPart) then return lhs

          val (opId, opSite) = operatorIdentifier()
          val lookahead = opId.get
          if lookahead.precedence < minPrecedence then 
              lhs
          else
            val op = lookahead
            val newRhs = loop2(ascribed(), op)
            val newLhs = InfixApplication(Identifier(op.toString, opSite), lhs, newRhs, lhs.site.extendedTo(lastBoundary))
            loop1(newLhs)
        case None | _ => 
          lhs

    @tailrec def loop2(rhs: Expression, op: OperatorIdentifier): Expression = 
      peek match
        case Some(t) =>
          if (!t.kind.isOperatorPart) then return rhs

          val backup = snapshot()
          val lookahead = operatorIdentifier()._1.get
          restore(backup)
          if lookahead.precedence <= op.precedence then 
            rhs
          else
            val additional = if lookahead.precedence > op.precedence then 1 else 0
            val newRHS = infixExpressionHelper(rhs, op.precedence + additional)
            loop2(newRHS, op)
        case None | _ =>
          rhs

    loop1(lhsGiven)

  /** Parses and returns an expression with an optional ascription. */
  private[parsing] def ascribed(): Expression =
    val expr = prefixExpression()
    peek match
      case Some(Token(K.At | K.AtQuery | K.AtBang, _)) =>
        val typeCast = typecast()
        val ascriptionType = tpe()
        AscribedExpression(expr, typeCast, ascriptionType, expr.site)
      case Some(_) | None =>
        expr

  /** Parses and returns a prefix application. */
  private[parsing] def prefixExpression(): Expression =
    peek match
      case Some(Token(K.Operator, s)) => // if it's an operator, get the operator identifier ("&" => AND, ...) and check if there is a space after
        val a = operatorIdentifier()
        val identifier = Identifier(a._1.get.toString, a._2)
        if (!noWhitespaceBeforeNextToken) then // if there is a space, no ambiguity, it's an operator so we return the Identifier
          identifier
        else // if there are no space, we backup and try to evaluate the token as a compoundExpression
          val backup = snapshot()
          compoundExpression() match
            case _ : ErrorTree => // if there is an error along the way, it means the token was an operator so we restore and return the Identifier
              restore(backup)
              identifier
            case other => // otherwise, there wasn't any error => it was a compoundExpression so we output the correct type
              PrefixApplication(identifier, other, identifier.site)
      case _ => compoundExpression() // honestly not sure about this

  /** Parses and returns a compound expression. */
  private[parsing] def compoundExpression(): Expression =
    val primaryExp = primaryExpression()
    compoundExpression2(primaryExp)

  private[parsing] def compoundExpression2(primaryExp : Expression): Expression =
    peek match
      case Some(Token(K.Dot, _)) => // check if the next token is a Dot (for selection)
        take()
        peek match
          case Some(Token(K.Identifier, _)) => // check if the next token is an Identifier
            val backup = snapshot()
            take()
            peek match
              case Some(Token(K.LParen, _)) => // check if the next token is a LParen (for method application)
                restore(backup)
                val id = identifier() // the identifier of the method
                val arguments = parenthesizedLabeledList(expression) :+ Labeled(None, primaryExp, primaryExp.site.extendedTo(lastBoundary))
                compoundExpression2(Application(id, arguments, primaryExp.site.extendedTo(lastBoundary)))
              case _ =>
                restore(backup)
                val id = identifier()
                compoundExpression2(Selection(primaryExp, id, primaryExp.site.extendedTo(lastBoundary)))
          case Some(Token(K.Integer, _)) => // check if the next token is an Integer
            val integ = integerLiteral()
            compoundExpression2(Selection(primaryExp, integ, primaryExp.site.extendedTo(lastBoundary)))
          case Some(Token(K.Operator, _)) => 
              operator() match
                case i : Identifier =>
                  compoundExpression2(Selection(primaryExp, i, primaryExp.site.extendedTo(lastBoundary)))
                case _ => 
                  report(SyntaxError("expected identifier or integer", emptySiteAtLastBoundary))
                  throw FatalError("expected identifier or integer", emptySiteAtLastBoundary)
          case _ =>     
            report(SyntaxError("expected identifier or integer", emptySiteAtLastBoundary))
            throw FatalError("expected identifier or integer", emptySiteAtLastBoundary)
              
      case Some(Token(K.LParen, _)) => // check if the next token is a LParen (for application)
        val arguments = parenthesizedLabeledList(expression)
        compoundExpression2(Application(primaryExp, arguments, primaryExp.site.extendedTo(lastBoundary)))
      case _ => primaryExp


  /** Parses and returns a term-level primary exression.
   *
   *  primary-expression ::=
   *    | value-identifier
   *    | integer-literal
   *    | float-literal
   *    | string-literal
   */
  private[parsing] def primaryExpression(): Expression =
    peek match
      case Some(Token(K.Identifier, s)) =>
        identifier()
      case Some(Token(K.True, _)) =>
        booleanLiteral()
      case Some(Token(K.False, _)) =>
        booleanLiteral()
      case Some(Token(K.Integer, _)) =>
        integerLiteral()
      case Some(Token(K.Float, _)) =>
        floatLiteral()
      case Some(Token(K.String, _)) =>
        stringLiteral()
      case Some(Token(K.Label, _)) =>
        recordExpression()
      case Some(Token(K.If, _)) =>
        conditional()
      case Some(Token(K.Match, _)) =>
        mtch()
      case Some(Token(K.Let, _)) =>
        let()
      case Some(Token(K.LParen, _)) =>
        lambdaOrParenthesizedExpression()
      case Some(t) if t.kind.isOperatorPart =>
        operator()
      case _ =>
        recover(ExpectedTree("expression", emptySiteAtLastBoundary), ErrorTree.apply)
  
  /** Parses and returns an Boolean literal expression. */
  private[parsing] def booleanLiteral(): BooleanLiteral =
    val s = expect("Boolean literal", K.True | K.False)
    BooleanLiteral(s.site.text.toString, s.site)

  /** Parses and returns an integer literal expression. */
  private[parsing] def integerLiteral(): IntegerLiteral =
    val s = expect(K.Integer)
    IntegerLiteral(s.site.text.toString, s.site)

  /** Parses and returns a floating-point literal expression. */
  private[parsing] def floatLiteral(): FloatLiteral =
    val s = expect(K.Float)
    FloatLiteral(s.site.text.toString, s.site)
    

  /** Parses and returns a string literal expression. */
  private[parsing] def stringLiteral(): StringLiteral =
    val s = expect(K.String)
    StringLiteral(s.site.text.toString, s.site)
    

  /** Parses and returns a term-level record expression. */
  private def recordExpression(): Record =
    record(() => recordExpressionFields(), (n, f, p) => Record(n, f, p))

  /** Parses and returns the fields of a term-level record expression. */
  private def recordExpressionFields(): List[Labeled[Expression]] =
    parenthesizedLabeledList(expression)

  /** Parses and returns a conditional expression. */
  private[parsing] def conditional(): Expression =
    val initial = expect(K.If)
    val cond = expression()
    val then_ = expect(K.Then)
    val success = expression()
    val else_ = expect(K.Else)
    val failure = expression()
    Conditional(cond, success, failure, initial.site.extendedTo(lastBoundary))

  /** Parses and returns a match expression. */
  private[parsing] def mtch(): Expression =
    val matchToken = expect(K.Match)
    val expr = expression()
    val mb = matchBody()
    Match(expr, mb, matchToken.site)

  /** Parses and returns a the cases of a match expression. */
  private def matchBody(): List[Match.Case] =
    @tailrec def loop(partialResult: List[Match.Case]): List[Match.Case] =
      peek match
        case Some(Token(K.RBrace, _)) =>
          partialResult
        case Some(Token(K.Case, _)) =>
          loop(partialResult :+ matchCase())
        case _ =>
          report(ExpectedTokenError(K.Case, emptySiteAtLastBoundary))
          discardUntilRecovery()
          if peek == None then partialResult else loop(partialResult)
    inBraces({ () => recovering(K.Case.matches, () => loop(List())) })

  /** Parses and returns a case in a match expression. */
  private def matchCase(): Match.Case =
    val s = peek.map((t) => t.site)
    if take(K.Case) == None then
      report(ExpectedTokenError(K.Case, emptySiteAtLastBoundary))
    val p = pattern()
    if take(K.Then) == None then
      recover(
        ExpectedTokenError(K.Then, emptySiteAtLastBoundary),
        (e) => Match.Case(p, ErrorTree(e), s.get.extendedTo(lastBoundary)))
    else
      val b = expression()
      Match.Case(p, b, s.get.extendedTo(lastBoundary))

  /** Parses and returns a let expression. */
  private[parsing] def let(): Let =
    val bind = binding()
    val body = inBraces(() => expression())
    Let(bind,body,bind.site.extendedTo(lastBoundary))

  /** Parses and returns a lambda or parenthesized term-level expression. */
  private def lambdaOrParenthesizedExpression(): Expression =
    val backup = snapshot()
    val start = take(K.LParen)
    if lambdafinding(backup) then
      val l = valueParameterList()
      val ty = peek match
        case Some(Token(K.Arrow, _)) =>
          take()
          Some(tpe())
        case _ =>
          None
      expect(K.LBrace)
      val body = expression()
      expect(K.RBrace)
      Lambda(l, ty, body, start.get.site.extendedTo(lastBoundary))
    else
      ParenthesizedExpression(inParentheses(expression), emptySiteAtLastBoundary)

  private def lambdafinding(backup: Parser.Snapshot): Boolean =
    peek match
      case Some(Token(K.RParen, _)) =>
        take()
        peek match
          case Some(Token(K.LBrace, _)) =>
            restore(backup)
            true
          case Some(_) | None =>
            restore(backup)
            false
      case Some(_) =>
        take()
        lambdafinding(backup)
      case None =>
        restore(backup)
        false

  /** Parses and returns an operator. */
  private def operator(): Expression =
    operatorIdentifier() match
      case (Some(o), p) => Identifier(p.text.toString, p)
      case (_, p) => ErrorTree(p)

  /** Parses and returns an operator identifier, along with its source positions.
   *
   *  If the the parsed operator is undefined, a diagnostic is reported and the returned identifier
   *  is `None`. In any case, the returned span represents the positions of the parsed identifier.
   */
  private def operatorIdentifier(): (Option[ast.OperatorIdentifier], SourceSpan) =
    import ast.OperatorIdentifier as O

    @tailrec def loop(start: Int, end: Int): (Option[ast.OperatorIdentifier], SourceSpan) =
      if takeIf((t) => t.isOperatorPartImmediatelyAfter(end)) != None then
        loop(start, lastBoundary)
      else
        val p = source.span(start, end)
        val s = p.text
        val o = if s == "||" then
          Some(O.LogicalOr)
        else if s == "&&" then
          Some(O.LogicalAnd)
        else if s == "<" then
          Some(O.LessThan)
        else if s == "<=" then
          Some(O.LessThanOrEqual)
        else if s == ">" then
          Some(O.GreaterThan)
        else if s == ">=" then
          Some(O.GreaterThanOrEqual)
        else if s == "==" then
          Some(O.Equal)
        else if s == "!=" then
          Some(O.NotEqual)
        else if s == "..." then
          Some(O.ClosedRange)
        else if s == "..<" then
          Some(O.HaflOpenRange)
        else if s == "+" then
          Some(O.Plus)
        else if s == "-" then
          Some(O.Minus)
        else if s == "|" then
          Some(O.BitwiseOr)
        else if s == "^" then
          Some(O.BitwiseXor)
        else if s == "*" then
          Some(O.Star)
        else if s == "/" then
          Some(O.Slash)
        else if s == "%" then
          Some(O.Percent)
        else if s == "&" then
          Some(O.Ampersand)
        else if s == "<<" then
          Some(O.LeftShift)
        else if s == ">>" then
          Some(O.RightShift)
        else if s == "~" then
          Some(O.Tilde)
        else if s == "!" then
          Some(O.Bang)
        else
          report(SyntaxError(s"undefined operator '${s}'", p))
          None
        (o, p)

    val h = expect("operator", (t) => t.kind.isOperatorPart)
    loop(h.site.start, h.site.end)

  /** Parses and returns a type cast operator. */
  private def typecast(): Typecast =
    peek match
      case Some(Token(K.At, _)) =>
        take(); Typecast.Widen
      case Some(Token(K.AtQuery, _)) =>
        take(); Typecast.Narrow
      case Some(Token(K.AtBang, _)) =>
        take(); Typecast.NarrowUnconditionally
      case _ =>
        throw FatalError("expected typecast operator", emptySiteAtLastBoundary)

  // --- Types ----------------------------------------------------------------

  /** Parses and returns a type-level expression. */
  private[parsing] def tpe(): Type =
    val l = tpeListHelper()
    if l.length == 1 then l.head else Sum(l, l.head.site.extendedTo(lastBoundary))

  private[parsing] def tpeListHelper(): List[Type] =
    val backup = snapshot()
    val t1 = primaryType()
    t1 match
      case ErrorTree(_) => 
        restore(backup)
        List(t1)
      case _ =>
        peek match
          case Some(Token(K.Operator, _)) if peek.get.site.text.toString == "|"=>
            take()
            tpeListHelper() match 
              case ErrorTree(_) :: next => 
                List(t1)
              case t => 
                List(t1) ::: t
          case _ =>
            List(t1)


  /** Parses and returns a type-level primary exression. */
  private def primaryType(): Type =
    peek match
      case Some(Token(K.Identifier, s)) =>
        typeIdentifier()
      case Some(Token(K.Label, _)) =>
        recordType()
      case Some(Token(K.LParen, _)) =>
        arrowOrParenthesizedType()
      case _ =>
        recover(ExpectedTree("type expression", emptySiteAtLastBoundary), ErrorTree.apply)

  /** Parses and returns a type identifier. */
  private def typeIdentifier(): Type =
    val s = expect(K.Identifier)
    TypeIdentifier(s.site.text.toString, s.site)

  /** Parses and returns a list of type arguments. */
  private def typeArguments(): List[Labeled[Type]] =
    List(Labeled(None, tpe(), emptySiteAtLastBoundary))

  /** Parses and returns a type-level record expressions. */
  private[parsing] def recordType(): RecordType =
    record(() => recordTypeFields(), (n, f, p) => RecordType(n, f, p))

  /** Parses and returns the fields of a type-level record expression. */
  private def recordTypeFields(): List[Labeled[Type]] =
    parenthesizedLabeledList(tpe)

  /** Parses and returns a arrow or parenthesized type-level expression. */
  private[parsing] def arrowOrParenthesizedType(): Type =
    val backup = snapshot()
    val arrow = arrowFindings(backup)
    if arrow then
      val l = parenthesizedLabeledList(tpe)
      take(K.Arrow)
      Arrow(l, tpe(), if l.isEmpty then emptySiteAtLastBoundary else l.head.site.extendedTo(lastBoundary))
    else
      ParenthesizedType(inParentheses(tpe), emptySiteAtLastBoundary)
  
  private[parsing] def arrowFindings(backup: Parser.Snapshot): Boolean =
    peek match
      case Some(Token(K.Arrow, _)) =>
        restore(backup)
        true
      case Some(value) =>
        take()
        arrowFindings(backup)
      case _ =>
        restore(backup)
        false

  // --- Patterns -------------------------------------------------------------

  /** Parses and returns a pattern. */
  private[parsing] def pattern(): Pattern =
    peek match
      case Some(Token(K.Underscore, _)) =>
        wildcard()
      case Some(Token(K.Label, _)) =>
        recordPattern()
      case Some(Token(K.Let, _)) =>
        bindingPattern()
      case _ =>
        valuePattern()

  /** Parses and returns a wildcard pattern. */
  def wildcard(): Wildcard =
    val s = expect(K.Underscore) // if it's an underscore, return a Wildcard with the site of the token
    Wildcard(s.site) // return the Wildcard

  /** Parses and returns a record pattern. */
  private def recordPattern(): RecordPattern =
    record(() => recordPatternFields(), (n, f, p) => RecordPattern(n, f, p))

  /** Parses and returns the fields of a record pattern. */
  private def recordPatternFields(): List[Labeled[Pattern]] =
    parenthesizedLabeledList(pattern)

  /** Parses and returns a binding pattern. */
  private def bindingPattern(): Binding =
    binding(false)

  /** Parses and returns a value pattern. */
  private def valuePattern(): ValuePattern =
    val e = expression()
    ValuePattern(e, e.site)

  // --- Common trees ---------------------------------------------------------

  /** Parses and returns an identifier. */
  private def identifier(): Identifier =
    val s = expect(K.Identifier)
    Identifier(s.site.text.toString, s.site)

  // --- Combinators ----------------------------------------------------------

  /** Parses and returns a record.
   *
   *  @param fields A closure parsing the fields of the record.
   *  @param make A closure constructing a record tree from its name, fields, and site.
   */
  private def record[Field <: Labeled[Tree], T <: RecordPrototype[Field]](
      fields: () => List[Field],
      make: (String, List[Field], SourceSpan) => T
  ): T =
    val label = expect(K.Label)
    //val id = identifier()
    peek match
      case Some(Token(K.LParen, _)) =>
        val fs = fields()
        make(label.site.text.toString, fs, label.site.extendedTo(lastBoundary))
      case _ =>
        make(label.site.text.toString, Nil, label.site.extendedTo(lastBoundary))

  /** Parses and returns a parenthesized list of labeled value.
   *
   *  See also [[this.labeledList]].
   *
   *  @param value A closure parsing a labeled value.
   */
  private[parsing] def parenthesizedLabeledList[T <: Tree](
      value: () => T
  ): List[Labeled[T]] =
    val isTerminator = K.RParen.matches
    inParentheses(() => commaSeparatedList(isTerminator, () => labeled(value)))

  /** Parses and returns a value optionally prefixed by a label.
   *
   *  This combinator attempts to parse a label `n` followed by a colon and then applies `value`.
   *  If that succeeds, returned tree is the result of `value` labeled by `n`. If there is no label,
   *  the combinator backtracks, re-applies `value`, and returns its result sans label.
   *
   *  @param value A closure parsing a labeled value.
   */
  private[parsing] def labeled[T <: Tree](
      value: () => T
  ): Labeled[T] =
    val backup = snapshot()
    val label = takeIf((a) => a.kind == K.Identifier || a.kind.isKeyword) // take if label is an Identifier or a Keyword
    label match 
      case Some(_) => // if it is, check if it's followed by a ':'
        take(K.Colon) match
          case None => // if it's not, it wasn't an Identifier but a value so backup and retrieve the value as a value
            restore(backup)
            val v = value()
            Labeled[T](None, v, v.site)
          case Some(_) => // if it is, get the value from the next token ('value()') and return the corresponding Labeled
            val v = value()
            Labeled(Some(label.get.site.text.toString), v, label.get.site.extendedTo(lastBoundary))

      case _ => // if it's neither, just return a Labeled with the value of the next token
        val v = value()
        Labeled[T](None, v, v.site)
      

  /** Parses and returns a sequence of `element` separated by commas and delimited on the RHS  by a
   *  token satisfying `isTerminator`.
   */
  private[parsing] def commaSeparatedList[T](isTerminator: Token => Boolean, element: () => T): List[T] =
    @tailrec def loop(partialResult: List[T]): List[T] =
      if peek.map(isTerminator).getOrElse(false) then
        partialResult
      else
        val nextPartialResult = partialResult :+ recovering(K.Comma.matches, element)
        if peek.map(isTerminator).getOrElse(false) then
          nextPartialResult
        else if take(K.Comma) != None then
          loop(nextPartialResult)
        else
          report(ExpectedTokenError(K.Comma, emptySiteAtLastBoundary))
          loop(nextPartialResult)
    loop(List())

  /** Parses and returns `element` surrounded by a pair of parentheses. */
  private[parsing] def inParentheses[T](element: () => T): T =
    val s = expect(K.LParen)
    val contents = recovering(K.RParen.matches, element)
    if take(K.RParen) == None then
      report(ExpectedTokenError(K.RParen, emptySiteAtLastBoundary))
    contents

  /** Parses and returns `element` surrounded by a pair of braces. */
  private[parsing] def inBraces[T](element: () => T): T =
    val s = expect(K.LBrace)
    val contents = recovering(K.RBrace.matches, element)
    if take(K.RBrace) == None then
      report(ExpectedTokenError(K.RBrace, emptySiteAtLastBoundary))
    contents

  /** Parses and returns `element` surrounded by angle brackets. */
  private[parsing] def inAngles[T](element: () => T): T =
    val s = expect(K.LAngle)
    val contents = recovering(K.RAngle.matches, element)
    if take(K.RAngle) == None then
      report(ExpectedTokenError(K.RAngle, emptySiteAtLastBoundary))
    contents

  /** Parses and returns `element` surrounded by a `left` and `right`. */
  private[parsing] def delimited[T](left: Token.Kind, right: Token.Kind, element: () => T): T =
    if take(left) == None then
      report(ExpectedTokenError(right, emptySiteAtLastBoundary))
    val contents = recovering(right.matches, element)
    if take(right) == None then
      report(ExpectedTokenError(right, emptySiteAtLastBoundary))
    contents

  /** Returns the result of `element` with `isRecoveryToken` added to the recovery predicates. */
  private def recovering[T](isRecoveryToken: Token => Boolean, element: () => T): T =
    recoveryPredicates += isRecoveryToken
    try
      element()
    finally
      recoveryPredicates.dropRightInPlace(1)

  // --- Utilities ------------------------------------------------------------

  /** Returns `true` iff there isn't any whitespace before the next token in the stream. */
  private def noWhitespaceBeforeNextToken: Boolean =
    peek.map((t) => lastBoundary == t.site.start).getOrElse(false)

  /** Reports a missing identifier and returns "_". */
  def missingName =
    report(ExpectedTokenError(K.Identifier, emptySiteAtLastBoundary))
    "_"

  /** Reports `error`, advances the stream to the next recovery token, and returns the result of
   *  calling `errorTree` on the skipped positions.
   */
  private def recover[T](error: SyntaxError, errorTree: SourceSpan => T): T =
    report(error)
    errorTree(discardUntilRecovery())

  /** Advances the stream to the next recovery token and returns the skipped positions. */
  private def discardUntilRecovery(): SourceSpan =
    @tailrec def loop(s: Int): SourceSpan =
      if !peek.isDefined || Reverse(recoveryPredicates).exists((p) => p(peek.get)) then
        source.span(s, lastBoundary)
      else
        take()
        loop(s)
    loop(lastBoundary)

  /** Consumes and returns the next token in the stream iff it has kind `k` or throw an exception
    * otherwise,
    */
  private def expect(construct: String, predicate: (Token) => Boolean): Token =
    takeIf(predicate) match
      case Some(next) => next
      case _ => throw FatalError(s"expected ${construct}", emptySiteAtLastBoundary)

  /** Consumes and returns the next token in the stream iff it has kind `k` or throw an exception
    * otherwise,
    */
  private def expect(k: Token.Kind): Token =
    take(k) match
      case Some(next) => next
      case _ => throw FatalError(s"expected ${k}", emptySiteAtLastBoundary)

  /** Returns the next token in the stream without consuming it. */
  private def peek: Option[Token] =
    if lookahead == None then
      lookahead = tokens.next()
    lookahead

  /** Consumes the next token in the stream iff it has kind `k` and returns the result of `action`
   *  applied on that token. */
  private def taking[T](k: Token.Kind, action: Token => T): Option[T] =
    take(k).map(action)

  /** Consumes and returns the next token in the stream. */
  private def take(): Option[Token] =
    peek.map({ (next) =>
      lastBoundary = next.site.end
      lookahead = None
      next
    })

  /** Consumes and returns the next token in the stream iff it has kind `k`. */
  private def take(k: Token.Kind): Option[Token] =
    takeIf(k.matches)

  /** Consumes and returns the next character in the stream iff it satisfies `predicate`. */
  private def takeIf(predicate: Token => Boolean): Option[Token] =
    if peek.map(predicate).getOrElse(false) then take() else None

  /** Returns an empty range at the position of the last consumed token. */
  private def emptySiteAtLastBoundary: SourceSpan =
    source.span(lastBoundary, lastBoundary)

  /** Reports the given diagnostic. */
  private def report(d: SyntaxError): Unit =
    errors += d

  /** Returns a backup of this instance's state. */
  private[parsing] def snapshot(): Parser.Snapshot =
    Parser.Snapshot(
      tokens.copy(), lastBoundary, lookahead, errors.length, recoveryPredicates.length)

  /** Restores this instance to state `s`. */
  private[parsing] def restore(s: Parser.Snapshot): Unit =
    tokens = s.tokens
    lastBoundary = s.lastBoundary
    lookahead = s.lookahead
    errors.dropRightInPlace(errors.length - s.errorCount)
    recoveryPredicates.dropRightInPlace(recoveryPredicates.length - s.recoveryPredicateCount)

end Parser

object Parser:

  /** The information necessary to restore the state of a parser instance. */
  private[parsing] final case class Snapshot(
      tokens: Lexer,
      lastBoundary: Int,
      lookahead: Option[Token],
      errorCount: Int,
      recoveryPredicateCount: Int)

end Parser

extension (self: Token.Kind) def | (other: Token.Kind): (Token) => Boolean =
  (t) => (t.kind == self) || (t.kind == other)

extension (self: Token => Boolean) def | (other: Token.Kind): (Token) => Boolean =
  (t) => self(t) || (t.kind == other)
