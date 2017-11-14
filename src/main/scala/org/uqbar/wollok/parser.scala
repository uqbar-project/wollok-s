package org.uqbar.wollok

import scala.util.parsing.combinator._
import model._
import sun.awt.X11.Separator
import sun.awt.X11.Separator

object parser extends RegexParsers {

  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
  // PARSER COMBINATORS
  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

  private implicit class ExtendedParser[T](parser: Parser[T]) {
    def *~[U](other: Parser[U]): Parser[List[T]] = repsep(parser, other)
    def *~(other: String): Parser[List[T]] = this *~ other
    def +~[U](other: Parser[U]): Parser[List[T]] = rep1sep(parser, other)
    def +~(other: String): Parser[List[T]] = this +~ other
    def *#(ammount: Int): Parser[List[T]] = repN(ammount, parser)
  }
  private implicit class ExtendedSeqParser[T](parser: Parser[Seq[T]]) {
    lazy val ?* = parser.? ^^ { _ getOrElse Nil }
  }

  private implicit class ExtendedString(string: String) {
    lazy val ?? = string.? ^^ { _.nonEmpty }
  }

  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
  // GRAMMAR
  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

  override protected val whiteSpace = """\/\*(?:(?!\*\/)[\s\S])*\*\/|\/\/[^\n]*\n?|[ \t\n]+""".r

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // COMMON
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  lazy val id: Parser[Id] = """^?[a-zA-Z_][a-zA-Z0-9_]*""".r

  lazy val localReference: Parser[LocalReference] = id
  lazy val fullyQualifiedReference: Parser[FullyQualifiedReference] = (id +~ ".") ~ ".*".??

  protected lazy val arguments: Parser[List[Expression]] = "(" ~> (expression *~ ",") <~ ")"
  protected lazy val parameters: Parser[List[Parameter]] = "(" ~> (parameter *~ ",") <~ ")"
  protected lazy val parameter: Parser[Parameter] = id ~ "...".??

  protected def block[T](content: Parser[T]): Parser[Seq[T]] = "{" ~> content *~ ";".? <~ "}"

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // TOP LEVEL
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  lazy val file: Parser[Package] = importStatement.* ~ packageMember.*
  protected lazy val packageMember: Parser[Member[Package]] = packageDef | singletonDef(true) | classDef | mixinDef | programDef | testDef
  protected lazy val importStatement: Parser[FullyQualifiedReference] = "import" ~> fullyQualifiedReference

  lazy val programDef: Parser[Program] = "program" ~> id ~ block(sentence)
  lazy val testDef: Parser[Test] = "test" ~> stringLiteral ~ block(sentence)
  lazy val packageDef: Parser[Package] = "package" ~> id ~ ("{" ~> packageMember.* <~ "}")
  lazy val classDef: Parser[Class] = "class" ~> id ~ ("inherits" ~> fullyQualifiedReference).? ~ mixinInclusion.?* ~ block(classMember)
  lazy val mixinDef: Parser[Mixin] = "mixin" ~> id ~ block(moduleMember)
  def singletonDef(named: Boolean): Parser[Singleton] = "object" ~> (if (named) id else "") ~ ("inherits" ~> fullyQualifiedReference ~ arguments.?*).? ~ mixinInclusion.?* ~ block(moduleMember)

  protected lazy val mixinInclusion: Parser[Seq[FullyQualifiedReference]] = "mixed with" ~> fullyQualifiedReference +~ ("and" | ",")

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // MODULE MEMBERS
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  protected lazy val classMember: Parser[Member[Class]] = constructor | moduleMember
  protected lazy val moduleMember: Parser[Member[Module]] = field | method

  lazy val constructor: Parser[Constructor] = "constructor" ~> parameters ~ ("=" ~> ("self" | "super") ~ arguments).? ~ block(sentence).?
  lazy val field: Parser[Field] = ("var" | "const") ~ id ~ ("=" ~> expression).?
  lazy val method: Parser[Method] = ("override".?? <~ "method") ~ (id | operator) ~ parameters ~ methodBody

  protected lazy val methodBody: Parser[(Boolean, Option[Seq[Sentence]])] = "native" ^^^ (true, None) | (block(sentence) | "=" ~> expression.*#(1)).? ^^ { false -> _ }

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // SENTENCES
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  lazy val sentence: Parser[Sentence] = variableStatement | returnStatement | assignmentStatement | expression

  lazy val variableStatement: Parser[Sentence] = ("var" | "const") ~ id ~ ("=" ~> expression).?
  lazy val returnStatement: Parser[Return] = "return" ~> expression
  lazy val assignmentStatement: Parser[Assignment] = localReference ~ assignmentOperator ~ expression

  protected lazy val assignmentOperator: Parser[String] = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | ">>>="

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // EXPRESSIONS
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  lazy val expression: Parser[Expression] = infixOperation()

  protected lazy val operator = (prefixOperator /: infixOperators) { _ | _ }
  protected lazy val prefixOperator = "not" | "!" | "-" | "+"
  protected lazy val infixOperators = List(
    "||" | "or",
    "&&" | "and",
    "===" | "!==" | "==" | "!=",
    ">=" | "<=" | ">" | "<",
    "..<" | ">.." | ".." | "->" | ">>>" | ">>" | "<<<" | "<<" | "<=>" | "<>" | "?:",
    "+" | "-",
    "**" | "*" | "/" | "%"
  )

  def infixOperation(precedenceLevel: Int = 0): Parser[Expression] = {
    val argument = precedenceLevel match {
      case n if n < infixOperators.size - 1  => infixOperation(n - 1)
      case n if n == infixOperators.size - 1 => prefixOperation
    }

    argument ~ (infixOperators(precedenceLevel) ~ argument).*
  }
  lazy val prefixOperation: Parser[Expression] = prefixOperator.* ~ messageChain

  lazy val messageChain: Parser[Expression] = primaryExpression ~ ("." ~> id ~ (arguments | closureLiteral ^^ { List(_) })).*

  protected lazy val primaryExpression: Parser[Expression] = self | superCall | constructorCall | ifExpression | throwExpression | tryExpression | literal | localReference | "(" ~> expression <~ ")"

  lazy val self = "self" ^^^ Self()
  lazy val superCall: Parser[Super] = "super" ~> arguments
  lazy val constructorCall: Parser[New] = "new" ~> fullyQualifiedReference ~ arguments
  lazy val ifExpression: Parser[If] = "if" ~> ("(" ~> expression <~ ")") ~ sentenceBlock ~ ("else" ~> sentenceBlock).?*
  lazy val throwExpression: Parser[Throw] = "throw" ~> expression
  lazy val tryExpression: Parser[Try] = "try" ~> sentenceBlock ~ catchClause.* ~ ("then always" ~> sentenceBlock).?*
  protected lazy val catchClause: Parser[Catch] = "catch" ~> parameter ~ (":" ~> fullyQualifiedReference).? ~ sentenceBlock
  protected lazy val sentenceBlock: Parser[Seq[Sentence]] = block(sentence) | sentence *# 1

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // LITERALS
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  protected lazy val literal: Parser[Expression] = nullLiteral | booleanLiteral | numberLiteral | stringLiteral | singletonDef(false) ^^ { Literal(_) } | closureLiteral | listLiteral | setLiteral

  lazy val nullLiteral = "null".r ^^^ Literal(null)
  lazy val booleanLiteral = "false|true".r ^^ { b => Literal(b == "true") }
  lazy val stringLiteral = (
    "\"" ~> "(\\b|\\t|\\n|\\f|\\r|\\u|\\\"|\\'|\\\\|[^\"\\])*".r <~ "\""
    | "'" ~> "(\\b|\\t|\\n|\\f|\\r|\\u|\\\"|\\'|\\\\|[^\"\\])*".r <~ "'"
  ) ^^ { Literal(_) }

  lazy val numberLiteral: Parser[Literal[AnyVal]] = (
    "[0x|0X][0-9a-fA-F]+".r ^^ { Integer.parseInt(_, 16) }
    | "[0-9]+.[0-9]+".r ^^ { _.toDouble }
    | "[0-9]+".r ^^ { Integer.parseInt(_) }
  ) ^^ { Literal(_) }

  lazy val listLiteral: Parser[New] = "[" ~ (expression *~ ",") <~ "]"
  lazy val setLiteral: Parser[New] = "#{" ~ (expression *~ ",") <~ "}"

  lazy val closureLiteral: Parser[Closure] = "{" ~> ((parameter *~ ",") <~ "=>").?* ~ sentence.*~(";".?) <~ "}"

  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
  // BUILDERS
  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

  private implicit def buildReference(p: Parser[Id]): Parser[LocalReference] = p ^^ { LocalReference(_) }

  private implicit def buildFullyQualifiedReference(p: Parser[List[Id] ~ Boolean]): Parser[FullyQualifiedReference] = p ^^ {
    case name ~ isGeneric => FullyQualifiedReference(name, isGeneric)
  }

  private implicit def buildParameter(p: Parser[Id ~ Boolean]): Parser[Parameter] = p ^^ {
    case name ~ hasVariableLength => Parameter(name, hasVariableLength)
  }

  private implicit def buildFile(p: Parser[List[FullyQualifiedReference] ~ List[Member[Package]]]): Parser[Package] = p ^^ {
    case imports ~ members => Package("", imports, members)
  }

  private implicit def buildProgram(p: Parser[Id ~ Seq[Sentence]]): Parser[Program] = p ^^ {
    case name ~ body => Program(name, body)
  }

  private implicit def buildTest(p: Parser[Literal[String] ~ Seq[Sentence]]): Parser[Test] = p ^^ {
    case description ~ body => Test(description, body)
  }

  private implicit def buildPackage(p: Parser[Id ~ List[Member[Package]]]): Parser[Package] = p ^^ {
    case name ~ members => Package(name, Nil, members)
  }

  private implicit def buildClass(p: Parser[Id ~ Option[FullyQualifiedReference] ~ Seq[FullyQualifiedReference] ~ Seq[Member[Class]]]): Parser[Class] = p ^^ {
    case name ~ superclass ~ mixins ~ members => Class(name, superclass, mixins, members)
  }

  private implicit def buildMixin(p: Parser[Id ~ Seq[Member[Module]]]): Parser[Mixin] = p ^^ {
    case name ~ members => Mixin(name, members)
  }

  private implicit def buildSingleton(p: Parser[Id ~ Option[FullyQualifiedReference ~ Seq[Expression]] ~ Seq[FullyQualifiedReference] ~ Seq[Member[Module]]]): Parser[Singleton] = p ^^ {
    case name ~ superclass ~ mixins ~ members => Singleton(name, superclass.map { case className ~ arguments => className -> arguments }, mixins, members)
  }

  private implicit def buildConstructor(p: Parser[Seq[Parameter] ~ Option[String ~ Seq[Expression]] ~ Option[Seq[Sentence]]]): Parser[Constructor] = p ^^ {
    case parameters ~ parentCall ~ body => Constructor(parameters, parentCall.fold(Seq[Expression]())(_._2), parentCall.fold(true)(_._1 == "true"), body)
  }

  private implicit def buildField(p: Parser[String ~ Id ~ Option[Expression]]): Parser[Field] = p ^^ {
    case readOnly ~ name ~ value => Field(name, readOnly == "const", value)
  }

  private implicit def buildMethod(p: Parser[Boolean ~ Id ~ List[Parameter] ~ (Boolean, Option[Seq[Sentence]])]): Parser[Method] = p ^^ {
    case isOverride ~ name ~ parameters ~ ((native, body)) => Method(name, isOverride, native, parameters, body)
  }

  private implicit def buildVariable(p: Parser[String ~ Id ~ Option[Expression]]): Parser[Variable] = p ^^ {
    case readOnly ~ name ~ value => Variable(name, readOnly == "const", value)
  }

  private implicit def buildReturn(p: Parser[Expression]): Parser[Return] = p ^^ { Return(_) }

  private implicit def buildAssignment(p: Parser[LocalReference ~ String ~ Expression]): Parser[Assignment] = p ^^ {
    case reference ~ "=" ~ value      => Assignment(reference, value)
    case reference ~ operator ~ value => Assignment(reference, Send(reference, operator dropRight 1, List(value)))
  }

  private implicit def buildInfixOperation(p: Parser[Expression ~ List[String ~ Expression]]): Parser[Expression] = p ^^ {
    case head ~ tail => (head /: tail) { case (left, op ~ right) => Send(left, op, List(right)) }
  }

  private implicit def buildPrefixOperation(p: Parser[List[String] ~ Expression]): Parser[Expression] = p ^^ {
    case ops ~ right => (right /: ops) { case (receiver, op) => Send(receiver, s"${op}_", Nil) }
  }

  private implicit def buildMessageChain(p: Parser[Expression ~ List[String ~ List[Expression]]]): Parser[Expression] = p ^^ {
    case head ~ tail => (head /: tail) { case (receiver, msg ~ args) => Send(receiver, msg, args) }
  }

  private implicit def buildSuper(p: Parser[List[Expression]]): Parser[Super] = p ^^ { Super(_) }

  private implicit def buildNew(p: Parser[FullyQualifiedReference ~ List[Expression]]): Parser[New] = p ^^ {
    case className ~ arguments => New(className, arguments)
  }

  private implicit def buildIf(p: Parser[Expression ~ Seq[Sentence] ~ Seq[Sentence]]): Parser[If] = p ^^ {
    case condition ~ trueBody ~ falseBody => If(condition, trueBody, falseBody)
  }

  private implicit def buildThrow(p: Parser[Expression]): Parser[Throw] = p ^^ { Throw(_) }

  private implicit def buildTry(p: Parser[Seq[Sentence] ~ List[Catch] ~ Seq[Sentence]]): Parser[Try] = p ^^ {
    case body ~ catches ~ always => Try(body, catches, always)
  }

  private implicit def buildCatch(p: Parser[Parameter ~ Option[FullyQualifiedReference] ~ Seq[Sentence]]): Parser[Catch] = p ^^ {
    case parameter ~ parameterType ~ body => Catch(parameter, parameterType, body)
  }

  private implicit def buildClosure(p: Parser[Seq[Parameter] ~ List[Sentence]]): Parser[Closure] = p ^^ {
    case arguments ~ body => Closure(arguments, body)
  }

  private implicit def buildCollection(p: Parser[String ~ List[Expression]]): Parser[New] = p ^^ {
    case "#{" ~ elements => New(FullyQualifiedReference("Set" :: Nil, false), elements)
    case _ ~ elements    => New(FullyQualifiedReference("List" :: Nil, false), elements)
  }

}