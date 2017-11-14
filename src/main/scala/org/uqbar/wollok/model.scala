package org.uqbar.wollok

object model {

  type Id = String

  sealed trait Node
  sealed trait Member[-T <: Node] extends Node
  sealed trait Module extends Member[Package]
  sealed trait Sentence extends Node
  sealed trait Expression extends Sentence

  case class LocalReference(name: Id, path: Option[Path[LocalReference]] = None) extends Expression
  case class FullyQualifiedReference(name: Seq[Id], isGeneric: Boolean, path: Option[Path[FullyQualifiedReference]] = None) extends Expression
  case class Parameter(name: Id, hasVariableLength: Boolean, path: Option[Path[Parameter]] = None) extends Node

  case class Package(name: Id, imports: Seq[FullyQualifiedReference], members: Seq[Member[Package]], path: Option[Path[Package]] = None) extends Member[Package]

  case class Program(name: Id, body: Seq[Sentence], path: Option[Path[Program]] = None) extends Member[Package]
  case class Test(description: Literal[String], body: Seq[Sentence], path: Option[Path[Test]] = None) extends Member[Package]

  case class Class(name: Id, superclass: Option[FullyQualifiedReference], mixins: Seq[FullyQualifiedReference], members: Seq[Member[Class]], path: Option[Path[Class]] = None) extends Module
  case class Mixin(name: Id, members: Seq[Member[Mixin]], path: Option[Path[Mixin]] = None) extends Module
  case class Singleton(name: Id, superclass: Option[(FullyQualifiedReference, Seq[Expression])], mixins: Seq[FullyQualifiedReference], members: Seq[Member[Singleton]], path: Option[Path[Singleton]] = None) extends Module

  case class Field(name: Id, isReadOnly: Boolean, value: Option[Expression], path: Option[Path[Field]] = None) extends Member[Module]
  case class Method(name: Id, isOverride: Boolean, isNative: Boolean, parameters: Seq[Parameter], body: Option[Seq[Sentence]], path: Option[Path[Method]] = None) extends Member[Module]
  case class Constructor(parameters: Seq[Parameter], baseArguments: Seq[Expression], callsSuper: Boolean, body: Option[Seq[Sentence]], path: Option[Path[Constructor]] = None) extends Member[Class]

  case class Variable(name: Id, isReadOnly: Boolean, value: Option[Expression], path: Option[Path[Variable]] = None) extends Sentence
  case class Return(value: Expression, path: Option[Path[Return]] = None) extends Sentence
  case class Assignment(reference: LocalReference, value: Expression, path: Option[Path[Assignment]] = None) extends Sentence

  case class Self(path: Option[Path[Self]] = None) extends Expression
  case class Literal[T](value: T, path: Option[Path[Literal[T]]] = None) extends Expression
  case class Closure(parameters: Seq[Parameter], body: Seq[Sentence], path: Option[Path[Closure]] = None) extends Expression
  case class Send(receiver: Expression, message: Id, arguments: Seq[Expression], path: Option[Path[Send]] = None) extends Expression
  case class Super(arguments: Seq[Expression], path: Option[Path[Super]] = None) extends Expression
  case class New(className: FullyQualifiedReference, arguments: Seq[Expression], path: Option[Path[New]] = None) extends Expression
  case class If(condition: Expression, thenBody: Seq[Sentence], elseBody: Seq[Sentence], path: Option[Path[If]] = None) extends Expression
  case class Throw(argument: Expression, path: Option[Path[Throw]] = None) extends Expression
  case class Try(body: Seq[Sentence], catches: Seq[Catch], always: Seq[Sentence], path: Option[Path[Try]] = None) extends Expression
  case class Catch(parameter: Parameter, parameterType: Option[FullyQualifiedReference], body: Seq[Sentence], path: Option[Path[Catch]] = None) extends Expression

  // Synthetic

  case class Environment(members: Seq[Member[Package]] = Nil) extends Node {
    val path = Some(Path("")(self => self))
  }

  case class Path[+N <: Node](name: String)(val apply: Environment => N) {
    def /[M <: Node](stepName: String)(step: N => M): Path[M] = this.copy(name + "." + stepName)(apply andThen step)
  }

}