package org.uqbar.wollok

import scala.language.implicitConversions
import scala.util.parsing.combinator.RegexParsers

import model.Assignment
import model.Catch
import model.Class
import model.Closure
import model.Constructor
import model.Expression
import model.Field
import model.FullyQualifiedReference
import model.If
import model.Import
import model.Literal
import model.LocalReference
import model.Member
import model.Method
import model.Mixin
import model.Module
import model.Name
import model.New
import model.Package
import model.Parameter
import model.Program
import model.Return
import model.Self
import model.Send
import model.Sentence
import model.Singleton
import model.Super
import model.Test
import model.Throw
import model.Try
import model.Variable

object Parser extends Parser

protected trait Parser extends RegexParsers {

  def apply(name: String, input: String) = parseFile(name, input)

  def parseFile(name: String, input: String) = parseAll(file, input)
  def parseSentence(input: String) = parseAll(sentence, input)
  def parseExpression(input: String) = parseAll(expression, input)

  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
  // GRAMMAR
  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

  override protected val whiteSpace = """\s*\/\*(?:(?!\*\/)[\s\S])*\*\/\s*|\s*\/\/[^\n]*\n?\s*|\s+""".r

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // COMMON
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  protected lazy val name: Parser[Name] = """\^?[a-zA-Z_][a-zA-Z0-9_]*""".r

  protected lazy val localReference: Parser[LocalReference] = name
  protected lazy val fullyQualifiedReference: Parser[FullyQualifiedReference] = name +~ "."

  private lazy val arguments: Parser[List[Expression]] = "(" ~> (expression *~ ",") <~ ")"
  private lazy val parameters: Parser[List[Parameter]] = "(" ~> (parameter *~ ",") <~ ")"
  private lazy val parameter: Parser[Parameter] = name ~ "...".??

  private def block[T](content: Parser[T]): Parser[Seq[T]] = "{" ~> (content *~ ";".?) <~ "}"

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // TOP LEVEL
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  protected lazy val file: Parser[Package] = importStatement.* ~ packageMember.*
  protected lazy val importStatement: Parser[Import] = "import" ~> fullyQualifiedReference ~ ".*".??
  private lazy val packageMember: Parser[Member[Package]] = packageDef | singletonDef(true) | classDef | mixinDef | programDef | testDef

  protected lazy val programDef: Parser[Program] = "program" ~> name ~ block(sentence)
  protected lazy val testDef: Parser[Test] = "test" ~> stringLiteral ~ block(sentence)
  protected lazy val packageDef: Parser[Package] = "package" ~> name ~ ("{" ~> packageMember.* <~ "}")
  protected lazy val classDef: Parser[Class] = "class" ~> name ~ ("inherits" ~> fullyQualifiedReference).? ~ mixinInclusion.?* ~ block(classMember)
  protected lazy val mixinDef: Parser[Mixin] = "mixin" ~> name ~ block(moduleMember)
  protected def singletonDef(named: Boolean): Parser[Singleton] = "object" ~> (if (named) name else "") ~ ("inherits" ~> fullyQualifiedReference ~ arguments.?*).? ~ mixinInclusion.?* ~ block(moduleMember)

  private lazy val mixinInclusion: Parser[Seq[FullyQualifiedReference]] = "mixed with" ~> (fullyQualifiedReference +~ ("and" | ",")) ^^ { _.reverse }

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // MODULE MEMBERS
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  private lazy val classMember: Parser[Member[Class]] = constructor | moduleMember
  private lazy val moduleMember: Parser[Member[Module]] = field | method

  protected lazy val constructor: Parser[Constructor] = "constructor" ~> parameters ~ ("=" ~> ("self" | "super") ~ arguments).? ~ block(sentence).?
  protected lazy val field: Parser[Field] = ("var" | "const") ~ name ~ ("=" ~> expression).?
  protected lazy val method: Parser[Method] = ("override".?? <~ "method") ~ (name | operator) ~ parameters ~ methodBody

  private lazy val methodBody: Parser[(Boolean, Option[Seq[Sentence]])] = "native" ^^^ (true, None) | (block(sentence) | "=" ~> expression.*#(1)).? ^^ { false -> _ }

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // SENTENCES
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  protected lazy val sentence: Parser[Sentence] = variableStatement | returnStatement | assignmentStatement | expression

  protected lazy val variableStatement: Parser[Variable] = ("var" | "const") ~ name ~ ("=" ~> expression).?
  protected lazy val returnStatement: Parser[Return] = "return" ~> expression
  protected lazy val assignmentStatement: Parser[Assignment] = localReference ~ assignmentOperator ~ expression

  private lazy val assignmentOperator: Parser[String] = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | ">>>="

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // EXPRESSIONS
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  protected lazy val expression: Parser[Expression] = infixOperation()

  private lazy val operator = (prefixOperator /: infixOperators) { _ | _ }
  private lazy val prefixOperator = "not" | "!" | "-" | "+"
  private lazy val infixOperators = List(
    "||" | "or",
    "&&" | "and",
    "===" | "!==" | "==" | "!=",
    ">=" | "<=" | ">" | "<",
    "..<" | ">.." | ".." | "->" | ">>>" | ">>" | "<<<" | "<<" | "<=>" | "<>" | "?:",
    "+" | "-",
    "**" | "*" | "/" | "%"
  )

  protected def infixOperation(precedenceLevel: Int = 0): Parser[Expression] = {
    val argument = precedenceLevel match {
      case n if n < infixOperators.size - 1  => infixOperation(n + 1)
      case n if n == infixOperators.size - 1 => prefixOperation
    }

    argument ~ (infixOperators(precedenceLevel) ~ argument).*
  }
  protected lazy val prefixOperation: Parser[Expression] = prefixOperator.* ~ messageChain

  protected lazy val messageChain: Parser[Expression] = primaryExpression ~ ("." ~> name ~ (arguments | closureLiteral ^^ { List(_) })).*

  private lazy val primaryExpression: Parser[Expression] = self | superCall | constructorCall | ifExpression | throwExpression | tryExpression | literal | localReference | "(" ~> expression <~ ")"

  protected lazy val self = "self" ^^^ Self
  protected lazy val superCall: Parser[Super] = "super" ~> arguments
  protected lazy val constructorCall: Parser[New] = "new" ~> fullyQualifiedReference ~ arguments
  protected lazy val ifExpression: Parser[If] = "if" ~> ("(" ~> expression <~ ")") ~ sentenceBlock ~ ("else" ~> sentenceBlock).?*
  protected lazy val throwExpression: Parser[Throw] = "throw" ~> expression
  protected lazy val tryExpression: Parser[Try] = "try" ~> sentenceBlock ~ catchClause.* ~ ("then always" ~> sentenceBlock).?*
  private lazy val catchClause: Parser[Catch] = "catch" ~> parameter ~ (":" ~> fullyQualifiedReference).? ~ sentenceBlock
  private lazy val sentenceBlock: Parser[Seq[Sentence]] = block(sentence) | sentence *# 1

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // LITERALS
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  protected lazy val literal: Parser[Expression] = nullLiteral | booleanLiteral | numberLiteral | stringLiteral | singletonDef(false) ^^ { Literal(_) } | closureLiteral | listLiteral | setLiteral

  private lazy val nullLiteral = "null".r ^^^ Literal(null)
  private lazy val booleanLiteral = "false|true".r ^^ { b => Literal(b == "true") }
  private lazy val stringLiteral = (
    "\"" ~> raw"""(\\b|\\t|\\n|\\f|\\r|\\u|\\\"|\\'|\\\\|[^"\\])*""".r <~ "\""
    | "'" ~> raw"""(\\b|\\t|\\n|\\f|\\r|\\u|\\\"|\\'|\\\\|[^'\\])*""".r <~ "'"
  ) ^^ { Literal(_) }

  private lazy val numberLiteral: Parser[Literal[AnyVal]] = (
    """\d+\.\d+""".r ^^ { _.toDouble }
    | """-?\d+""".r ^^ { _.toInt }
  ) ^^ { Literal(_) }

  private lazy val listLiteral: Parser[New] = "[" ~ (expression *~ ",") <~ "]"
  private lazy val setLiteral: Parser[New] = "#{" ~ (expression *~ ",") <~ "}"
  private lazy val closureLiteral: Parser[Literal[Singleton]] = "{" ~> ((parameter *~ ",") <~ "=>").?* ~ sentence.*~(";".?) <~ "}"

  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
  // PARSER COMBINATORS
  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

  private implicit class ExtendedParser[T](parser: Parser[T]) {
    def *~[U](other: Parser[U]): Parser[List[T]] = repsep(parser, other)
    def *~(other: String): Parser[List[T]] = repsep(parser, other)
    def +~[U](other: Parser[U]): Parser[List[T]] = rep1sep(parser, other)
    def +~(other: String): Parser[List[T]] = rep1sep(parser, other)
    def *#(ammount: Int): Parser[List[T]] = repN(ammount, parser)
  }

  private implicit class ExtendedSeqParser[T](parser: Parser[Seq[T]]) {
    lazy val ?* = parser.? ^^ { _ getOrElse Nil }
  }

  private implicit class ExtendedString(string: String) {
    lazy val ?? = string.? ^^ { _.nonEmpty }
  }

  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
  // BUILDERS
  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

  private implicit def buildReference(p: Parser[Name]): Parser[LocalReference] = p ^^ { LocalReference(_) }

  private implicit def buildFullyQualifiedReference(p: Parser[List[Name]]): Parser[FullyQualifiedReference] = p ^^ {
    case name => FullyQualifiedReference(name)
  }

  private implicit def buildImport(p: Parser[FullyQualifiedReference ~ Boolean]): Parser[Import] = p ^^ {
    case reference ~ isGeneric => Import(reference, isGeneric)
  }

  private implicit def buildParameter(p: Parser[Name ~ Boolean]): Parser[Parameter] = p ^^ {
    case name ~ hasVariableLength => Parameter(name, hasVariableLength)
  }

  private implicit def buildFile(p: Parser[List[Import] ~ List[Member[Package]]]): Parser[Package] = p ^^ {
    case imports ~ members => Package("", imports, members)
  }

  private implicit def buildProgram(p: Parser[Name ~ Seq[Sentence]]): Parser[Program] = p ^^ {
    case name ~ body => Program(name, body)
  }

  private implicit def buildTest(p: Parser[Literal[String] ~ Seq[Sentence]]): Parser[Test] = p ^^ {
    case description ~ body => Test(description, body)
  }

  private implicit def buildPackage(p: Parser[Name ~ List[Member[Package]]]): Parser[Package] = p ^^ {
    case name ~ members => Package(name, Nil, members)
  }

  private implicit def buildClass(p: Parser[Name ~ Option[FullyQualifiedReference] ~ Seq[FullyQualifiedReference] ~ Seq[Member[Class]]]): Parser[Class] = p ^^ {
    case name ~ superclass ~ mixins ~ members => Class(name, superclass, mixins, members)
  }

  private implicit def buildMixin(p: Parser[Name ~ Seq[Member[Module]]]): Parser[Mixin] = p ^^ {
    case name ~ members => Mixin(name, members)
  }

  private implicit def buildSingleton(p: Parser[Name ~ Option[FullyQualifiedReference ~ Seq[Expression]] ~ Seq[FullyQualifiedReference] ~ Seq[Member[Module]]]): Parser[Singleton] = p ^^ {
    case name ~ superclass ~ mixins ~ members => Singleton(name, superclass.map { case className ~ arguments => className -> arguments }, mixins, members)
  }

  private implicit def buildConstructor(p: Parser[Seq[Parameter] ~ Option[String ~ Seq[Expression]] ~ Option[Seq[Sentence]]]): Parser[Constructor] = p ^^ {
    case parameters ~ parentCall ~ body => Constructor(parameters, parentCall.fold(Seq[Expression]())(_._2), parentCall.fold(true)(_._1 == "super"), body)
  }

  private implicit def buildField(p: Parser[String ~ Name ~ Option[Expression]]): Parser[Field] = p ^^ {
    case readOnly ~ name ~ value => Field(name, readOnly == "const", value)
  }

  private implicit def buildMethod(p: Parser[Boolean ~ Name ~ List[Parameter] ~ (Boolean, Option[Seq[Sentence]])]): Parser[Method] = p ^^ {
    case isOverride ~ name ~ parameters ~ ((native, body)) => Method(name, isOverride, native, parameters, body)
  }

  private implicit def buildVariable(p: Parser[String ~ Name ~ Option[Expression]]): Parser[Variable] = p ^^ {
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
    case ops ~ Literal(right: Int) if ops.lastOption == Some("-") => (ops.init :\ (Literal(-right): Expression)) { case (op, receiver) => Send(receiver, op, Nil) }
    case ops ~ Literal(right: Double) if ops.lastOption == Some("-") => (ops.init :\ (Literal(-right): Expression)) { case (op, receiver) => Send(receiver, op, Nil) }
    case ops ~ right => (ops :\ right) { case (op, receiver) => Send(receiver, op, Nil) }
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

  private implicit def buildClosure(p: Parser[Seq[Parameter] ~ List[Sentence]]): Parser[Literal[Singleton]] = p ^^ {
    case parameters ~ body => Closure(parameters, body)
  }

  private implicit def buildCollection(p: Parser[String ~ List[Expression]]): Parser[New] = p ^^ {
    case "#{" ~ elements => New("wollok.Set", elements)
    case _ ~ elements    => New("wollok.List", elements)
  }

}