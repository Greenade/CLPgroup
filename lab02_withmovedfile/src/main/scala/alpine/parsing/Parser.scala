package alpine
package parsing

import alpine.ast
import alpine.util.FatalError

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.SeqView.Reverse

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
          case Some(Token(K.Eq, _)) =>
            take()
            val initializer = Some(expression())
            Binding(identifier.toString,ascription,initializer,identifier.site.extendedTo(lastBoundary))

          case _ if initializerIsExpected =>
            throw FatalError("expected initializer", emptySiteAtLastBoundary)

          case _ =>
           Binding(identifier.toString,ascription,None,identifier.site.extendedTo(lastBoundary))

      case Some(Token(K.Eq, _)) =>
        take()
        val initializer = Some(expression())
        Binding(identifier.toString,None,initializer,identifier.site.extendedTo(lastBoundary))

      case _ if initializerIsExpected =>
        throw FatalError("expected initializer", emptySiteAtLastBoundary)

      case _ =>
        Binding(identifier.toString,None,None,identifier.site.extendedTo(lastBoundary))


  /** Parses and returns a function declaration. */
  private[parsing] def function(): Function =
    ???

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
    ???

  /** Parses and returns a parameter declaration. */
  private[parsing] def parameter(): Declaration =
    ???

  /** Parses and returns a type declaration. */
  private[parsing] def typeDeclaration(): TypeDeclaration =
    ???

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
  private[parsing] def infixExpressionHelper(lhsGiven : Expression, precedence: Int = ast.OperatorPrecedence.min): Expression =
    val backup = snapshot()
    var lhs = lhsGiven // mutable left hand side
    var lookAhead = operatorIdentifier()._1.getOrElse({restore(backup);lhs})

    while ???/*lookAhead.precedence >= precedence*/ do
      val op = lookAhead
      var rhs = expression() // not sure ??
      val backup = snapshot()
      lookAhead = operatorIdentifier()._1.getOrElse({restore(backup);lhs})

      while ???/*lookAhead.precedence > precedence*/ do
        val newPrecedence = ??? /*op.precedence + (if lookAhead.precedence > op.precedence then 1 else 0)*/
        rhs = infixExpressionHelper(rhs,newPrecedence)
        val backup = snapshot()
        lookAhead = operatorIdentifier()._1.getOrElse({restore(backup);lhs})
      lhs = ???//BinaryOperation(op, lhs, rhs, op.site.extendedTo(lastBoundary))
    lhs

  /** Parses and returns an expression with an optional ascription. */
  private[parsing] def ascribed(): Expression =
    ???

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
                  throw FatalError("expected identifier or integer", emptySiteAtLastBoundary)
          case _ =>     
            throw FatalError("expected identifier or integer", emptySiteAtLastBoundary)
              
      case Some(Token(K.LParen, _)) => // check if the next token is a LParen (for application)
        take()
        val arguments = parenthesizedLabeledList(() => expression())
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
    val s = expect(K.Label)
    record(() => recordExpressionFields(), (n, f, p) => Record(n, f, p))

  /** Parses and returns the fields of a term-level record expression. */
  private def recordExpressionFields(): List[Labeled[Expression]] =
    val fields = parenthesizedLabeledList(() => labeled(expression))
    fields.map((f) => f.asInstanceOf[Labeled[Expression]])

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
    ???

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
    ???

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
    peek match
      case Some(Token(K.Identifier, _)) =>
        var ty = primaryType()
        peek match
          case Some(Token(K.Operator, _)) =>
            take()
            var s2 = peek.get.site
            var ty2 = primaryType()
            peek match
              case Some(Token(K.Operator, _)) =>
                take()
                var s3 = peek.get.site
                var ty3 = primaryType()
                Sum(List(ty,ty2,ty3),s3)
              case _ => 
                Sum(List(ty,ty2),s2)
          case _ =>
            ty
      case _ =>
        primaryType()

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
    ???

  /** Parses and returns a type-level record expressions. */
  private[parsing] def recordType(): RecordType =
    ???

  /** Parses and returns the fields of a type-level record expression. */
  private def recordTypeFields(): List[Labeled[Type]] =
    ???

  /** Parses and returns a arrow or parenthesized type-level expression. */
  private[parsing] def arrowOrParenthesizedType(): Type =
    ???

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
    take() // consume the token
    Wildcard(s.site) // return the Wildcard

  /** Parses and returns a record pattern. */
  private def recordPattern(): RecordPattern =
    ???

  /** Parses and returns the fields of a record pattern. */
  private def recordPatternFields(): List[Labeled[Pattern]] =
    ???


  /** Parses and returns a binding pattern. */
  private def bindingPattern(): Binding =
    ???

  /** Parses and returns a value pattern. */
  private def valuePattern(): ValuePattern =
    ???

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
    ???
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
