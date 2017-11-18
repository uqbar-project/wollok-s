package org.uqbar.wollok

import scala.language.implicitConversions

object model {

  type Name = String

  sealed trait Node {
    val id = Id.next

    def parent(implicit environment: Environment): Option[Node] = environment.parenthood.get(id) flatMap { parentId => environment(parentId) }
    def scope(implicit environment: Environment): Map[Name, Node] = environment.scopes(id).mapValues { id => environment(id).get }

    def mapAccum[T](acum: T, acumTx: (T, Node) => T)(tx: (T, Node) => Node): (T, this.type) = {
      val next = tx(acum, this)
      val childAcum = acumTx(acum, next)
      def continue[N <: Node](n: N) = n.mapAccum(childAcum, acumTx)(tx)._2

      childAcum -> (next match {
        case node: Environment => node.copy(members = node.members map continue)
        case node: Package => node.copy(members = node.members map continue, imports = node.imports map continue)
        case node: Program => node.copy(body = node.body map continue)
        case node: Test => node.copy(body = node.body map continue)
        case node: Class => node.copy(members = node.members map continue, mixins = node.mixins map continue, superclass = node.superclass map continue)
        case node: Mixin => node.copy(members = node.members map continue)
        case node: Singleton => node.copy(members = node.members map continue, mixins = node.mixins map continue, superclass = node.superclass map { case (name, args) => continue(name) -> (args map continue) })
        case node: Field => node.copy(value = node.value map continue)
        case node: Method => node.copy(parameters = node.parameters map continue, body = node.body map { _ map continue })
        case node: Constructor => node.copy(parameters = node.parameters map continue, body = node.body map { _ map continue }, baseArguments = node.baseArguments map continue)
        case node: Variable => node.copy(value = node.value map continue)
        case node: Return => node.copy(value = continue(node.value))
        case node: Assignment => node.copy(reference = continue(node.reference), value = continue(node.value))
        case node: Send => node.copy(receiver = continue(node.receiver), arguments = node.arguments map continue)
        case node: Super => node.copy(arguments = node.arguments map continue)
        case node: New => node.copy(className = continue(node.className), arguments = node.arguments map continue)
        case node: If => node.copy(condition = continue(node.condition), thenBody = node.thenBody map continue, elseBody = node.elseBody map continue)
        case node: Throw => node.copy(continue(node.argument))
        case node: Try => node.copy(body = node.body map continue, catches = node.catches map continue, always = node.always map continue)
        case node: Catch => node.copy(parameter = continue(node.parameter), parameterType = node.parameterType map continue, body = node.body map continue)
        case node @ Literal(value: Singleton) => node.copy(value = continue(value))
        case Self | _: Literal[_] | _: LocalReference | _: FullyQualifiedReference | _: Import | _: Parameter => next
      }).asInstanceOf[this.type]
    }

    def map(f: PartialFunction[Node, Node]): this.type = {
      val fallback: PartialFunction[Node, Node] = { case n => n }
      mapAccum[Unit]((), { case (a, _) => a }) { case (_, n) => (f orElse fallback)(n) }._2
    }

    def children = this match {
      case node: Environment => node.members
      case node: Package => node.members ++ node.imports
      case node: Program => node.body
      case node: Test => node.body
      case node: Class => node.members ++ node.mixins ++ node.superclass.toList
      case node: Mixin => node.members
      case node: Singleton => node.members ++ node.mixins ++ node.superclass.fold(Seq[Node]()) { case (name, args) => name +: args }
      case node: Field => node.value.toList
      case node: Method => node.parameters ++ node.body.getOrElse(Nil)
      case node: Constructor => node.parameters ++ node.body.getOrElse(Nil) ++ node.baseArguments
      case node: Variable => node.value.toList
      case node: Return => node.value :: Nil
      case node: Assignment => node.reference :: node.value :: Nil
      case node: Send => node.receiver +: node.arguments
      case node: Super => node.arguments
      case node: New => node.className +: node.arguments
      case node: If => (node.condition +: node.thenBody) ++ node.elseBody
      case node: Throw => node.argument :: Nil
      case node: Try => node.body ++ node.catches ++ node.always.toList
      case node: Catch => (node.parameter +: node.parameterType.toList) ++ node.body
      case node @ Literal(value: Singleton) => value :: Nil
      case Self | _: Literal[_] | _: LocalReference | _: FullyQualifiedReference | _: Import | _: Parameter => Nil
    }
  }

  sealed trait Referenceable extends Node { def name: Name }
  sealed trait Member[-T <: Node] extends Node
  sealed trait Module extends Member[Package] with Referenceable
  sealed trait Sentence extends Node
  sealed trait Expression extends Sentence
  sealed trait Reference extends Expression { def target(implicit environment: Environment): Node }

  case class Import(reference: FullyQualifiedReference, isGeneric: Boolean = false) extends Node
  case class Parameter(name: Name, hasVariableLength: Boolean = false) extends Referenceable

  case class LocalReference(name: Name) extends Reference {
    def target(implicit environment: Environment): Node = scope.apply(this.name)
  }

  object FullyQualifiedReference {
    implicit def fromString(s: String) = FullyQualifiedReference(s.split('.').toList)
  }
  case class FullyQualifiedReference(name: Seq[Name]) extends Reference {
    def target(implicit environment: Environment): Node = environment(this).get
  }

  case class Package(name: Name, imports: Seq[Import] = Nil, members: Seq[Member[Package]] = Nil) extends Member[Package] with Referenceable

  case class Program(name: Name, body: Seq[Sentence] = Nil) extends Member[Package]
  case class Test(description: Literal[String], body: Seq[Sentence] = Nil) extends Member[Package]

  case class Class(name: Name, superclass: Option[FullyQualifiedReference] = None, mixins: Seq[FullyQualifiedReference] = Nil, members: Seq[Member[Class]] = Nil) extends Module
  case class Mixin(name: Name, members: Seq[Member[Mixin]] = Nil) extends Module
  case class Singleton(name: Name, superclass: Option[(FullyQualifiedReference, Seq[Expression])] = None, mixins: Seq[FullyQualifiedReference] = Nil, members: Seq[Member[Singleton]] = Nil) extends Module

  case class Field(name: Name, isReadOnly: Boolean, value: Option[Expression] = None) extends Member[Module] with Referenceable
  case class Method(name: Name, isOverride: Boolean = false, isNative: Boolean = false, parameters: Seq[Parameter] = Nil, body: Option[Seq[Sentence]] = Some(Nil)) extends Member[Module]
  case class Constructor(parameters: Seq[Parameter] = Nil, baseArguments: Seq[Expression] = Nil, callsSuper: Boolean = true, body: Option[Seq[Sentence]] = Some(Nil)) extends Member[Class]

  case class Variable(name: Name, isReadOnly: Boolean, value: Option[Expression] = None) extends Sentence with Referenceable
  case class Return(value: Expression) extends Sentence
  case class Assignment(reference: LocalReference, value: Expression) extends Sentence

  case object Self extends Expression
  case class Literal[T](value: T) extends Expression
  case class Send(receiver: Expression, message: Name, arguments: Seq[Expression] = Nil) extends Expression
  case class Super(arguments: Seq[Expression] = Nil) extends Expression
  case class New(className: FullyQualifiedReference, arguments: Seq[Expression] = Nil) extends Expression
  case class If(condition: Expression, thenBody: Seq[Sentence] = Nil, elseBody: Seq[Sentence] = Nil) extends Expression
  case class Throw(argument: Expression) extends Expression
  case class Try(body: Seq[Sentence] = Nil, catches: Seq[Catch] = Nil, always: Seq[Sentence] = Nil) extends Expression
  case class Catch(parameter: Parameter, parameterType: Option[FullyQualifiedReference] = None, body: Seq[Sentence] = Nil) extends Node

  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
  // SYNTHETICS
  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

  type Id = Long
  object Id {
    private var current: Id = 0
    def next: Id = {
      current += 1
      current
    }
  }

  def Closure(parameters: Seq[Parameter] = Nil, body: Seq[Sentence] = Nil) = Literal(Singleton("", members = Method("call", parameters = parameters, body = Some(body)) :: Nil))

  case class Environment(members: Seq[Member[Package]] = Nil) extends Node {
    implicit val self = this

    def apply[T <: Node](id: Id) = cache.get(id).asInstanceOf[Option[T]]
    def apply[T <: Node](fqr: FullyQualifiedReference) = (Option[Node](this) /: fqr.name) { case (parent, step) => parent.flatMap { _.children.collectFirst { case child: Referenceable if child.name == step => child } } }.asInstanceOf[Option[T]]

    lazy val cache: Map[Id, Node] = {
      def entries(node: Node): Seq[(Id, Node)] = (node.id -> node) +: node.children.flatMap(entries)
      entries(this).toMap
    }

    lazy val parenthood: Map[Id, Id] = {
      def entries(node: Node): Seq[(Id, Id)] = node.children.map { _.id -> node.id } ++ node.children.flatMap(entries)
      entries(this).toMap
    }

    lazy val scopes: Map[Id, Map[Name, Id]] = {
      def scopeContributions(node: Node): Map[Name, Id] = node match {
        case node: Referenceable            => Map(node.name -> node.id)
        case node: Import if node.isGeneric => apply[Node](node.reference).get.children.flatMap(scopeContributions).toMap
        case node: Import                   => scopeContributions(this(node.reference).get)
        case _                              => Map()
      }
      def entries(inheritedScope: Map[Name, Id])(node: Node): Map[Id, Map[Name, Id]] = Map(node.id -> inheritedScope) ++ node.children.flatMap(entries(inheritedScope ++ node.children.flatMap(scopeContributions)))
      val wre = members.collectFirst { case wre: Package if wre.name == "wollok" => wre }.map(scopeContributions)
      entries(wre getOrElse Map())(this)
    }
  }

}

// TODO: Validaciones para el modelo
// - Sentencias después de un return
// - If sin then ni else
// - No más de un parámetro puede ser vararg
// - Sólo el último parámetro puede ser vararg
// - No vararg en el catch
// - No más de un catch sin tipo de parámetro
// - Catch sin tipo es el último
// - Try sin catch ni always
// - Variables/Fields no referenciados
// - Variables/Fields inmutables no inicializados
// - LocalReferences fuera de scope
// - FullyQualifiedReferences no en el ambiente

