package org.uqbar.wollok

object model {

  type Id = String

  sealed trait Node
  sealed trait Member[+T <: Node] extends Node
  sealed trait Module extends Member[Package]
  sealed trait Sentence extends Node
  sealed trait Expression extends Sentence

  case class Reference(name: Id) extends Expression
  case class FullyQualifiedReference(path: Seq[Id]) extends Expression
  case class Block(sentences: Seq[Sentence]) extends Node
  case class Parameter(name: Id, hasVariableLength: Boolean) extends Node

  case class Package(name: Id, imports: Seq[Reference], members: Seq[Member[Package]]) extends Member[Package]

  case class Program(name: Id, body: Block) extends Member[Package]
  case class Test(description: String, body: Block) extends Member[Package]

  case class Class(name: Id, superclass: FullyQualifiedReference, mixins: Seq[FullyQualifiedReference], members: Seq[Member[Class]]) extends Module
  case class Mixin(name: Id, members: Seq[Member[Mixin]]) extends Module
  case class Singleton(name: Id, superclass: FullyQualifiedReference, superArguments: Seq[Expression], mixins: Seq[FullyQualifiedReference], members: Seq[Member[Singleton]]) extends Module

  case class Field(name: Id, isReadOnly: Boolean, value: Expression) extends Member[Module]
  case class Method(name: Id, isOverride: Boolean, isNative: Boolean, parameters: Seq[Parameter], body: Block) extends Member[Module]
  case class Constructor(parameters: Seq[Parameter], baseArguments: Seq[Expression], callsSuper: Boolean, body: Block) extends Member[Class]

  case class Variable(name: Id, isReadOnly: Boolean, value: Expression) extends Sentence
  case class Return(value: Expression) extends Sentence
  case class Assignment(reference: Reference, value: Expression) extends Sentence

  case object Self extends Expression
  case class Literal[T](value: T) extends Expression
  case class Closure(parameters: Seq[Parameter], body: Block) extends Expression
  case class Send(receiver: Expression, message: Id, parameters: Seq[Parameter]) extends Expression
  case class Super(parameters: Seq[Parameter]) extends Expression
  case class New(className: FullyQualifiedReference, parameters: Seq[Parameter]) extends Expression
  case class If(condition: Expression, thenBody: Block, elseBody: Block) extends Expression
  case class Throw(exception: Expression) extends Expression
  case class Try(body: Block, catches: Seq[Catch], always: Block) extends Expression
  case class Catch(parameter: Parameter, parameterType: FullyQualifiedReference, body: Block) extends Expression

  // Synthetic

  case class Environment(members: Seq[Package]) extends Node

}