package org.uqbar.wollok

import scala.language.implicitConversions

object model {

  type Name = String

  sealed trait Node {
    lazy val id = Id.next

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
      case Literal(value: Singleton) => value :: Nil
      case Self | _: Literal[_] | _: LocalReference | _: FullyQualifiedReference | _: Import | _: Parameter => Nil
    }
  }

  sealed trait Referenceable extends Node { def name: Name }
  sealed trait Member[-T <: Node] extends Node
  sealed trait Module extends Member[Package] with Referenceable { def members: Seq[Member[this.type]] }

  sealed trait Sentence extends Node
  sealed trait Expression extends Sentence
  //TODO: Reference[T]
  sealed trait Reference extends Expression

  case class Import(reference: FullyQualifiedReference, isGeneric: Boolean = false) extends Node
  case class Parameter(name: Name, hasVariableLength: Boolean = false) extends Referenceable

  case class LocalReference(name: Name) extends Reference
  case class FullyQualifiedReference(name: Seq[Name]) extends Reference

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
    implicit val self: Environment = this

    def apply[T <: Node](id: Id): T = cache(id).asInstanceOf[T]
    def apply[T <: Node](fqr: FullyQualifiedReference): T = fqr.target.asInstanceOf[T]
    def apply[T <: Node](fqr: String): T = ((this: Node) /: fqr.split('.')){ case (parent, step) =>
      parent.children.collectFirst { case c: Referenceable if c.name == step => c }.getOrElse {throw new RuntimeException(s"Reference to missing module $fqr") }
    }.asInstanceOf[T]

    lazy val cache: Map[Id, Node] = {
      def entries(node: Node): Seq[(Id, Node)] = (node.id -> node) +: node.children.flatMap(entries)
      entries(this).toMap
    }

    lazy val parenthood: Map[Id, Id] = {
      def entries(node: Node): Seq[(Id, Id)] = node.children.map { _.id -> node.id } ++ node.children.flatMap(entries)
      entries(this).toMap
    }

    private val scopes = collection.mutable.Map[Id, Map[Name, Id]]()
    def scopeFor(node: Node): Map[Name,Id] = {

      def scopeContributions(node: Node): Map[Name, Id] = node match {
        case node: Singleton                => if (node.name != "") Map(node.name -> node.id) else Map()
        case node: Package if node.name == "wollok" => Map((node.name -> node.id) +: node.children.flatMap(scopeContributions):_*)
        case node: Referenceable            => Map(node.name -> node.id)
        case _                              => Map()
      }

      def scopeEntries(node: Node): Map[Name,Id] = Map(node.parent.map { parent =>
        val entriesFromParent: Seq[(Name,Id)] = scopeFor(parent).toSeq
        val entriesFromSiblings: Seq[(Name,Id)] = parent.children.flatMap(scopeContributions)
        val entriesFromAncestors: Seq[(Name,Id)] = parent match {
          case node: Module => node.allMembers.flatMap(scopeContributions)
          case _ => Nil
        }

        entriesFromParent ++ entriesFromSiblings ++ entriesFromAncestors
      }.getOrElse(Nil) : _*)

      scopes.getOrElseUpdate(node.id, scopeEntries(node))
    }
  }

  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
  // RUNTIME EXTENSIONS
  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

  implicit class RuntimeNode(node: Node)(implicit environment: Environment) {
    def parent = environment.parenthood.get(node.id) map { parentId => environment[Node](parentId) }
    def scope = environment.scopeFor(node).mapValues { id => environment[Node](id) }
  }

  implicit class RuntimeReference(reference: Reference)(implicit environment: Environment) {
    def target = reference match {
      case LocalReference(name) => reference.scope.getOrElse(name, throw new RuntimeException(s"Reference to missing local $name"))
      case FullyQualifiedReference(name) => {
        //TODO: Temp code. Make it better.

        def contextPackage(node:Node): Package = node match {
          case p: Package => p
          case _ => contextPackage(node.parent.get)
        }

        def base(where: Package): Option[Referenceable] =
          where.children.collectFirst{ case child: Referenceable if child.name == name.head => child }.orElse(
            where.imports.collectFirst{
              case Import(imported,true) if environment[Package](imported).children.collectFirst{ case child: Referenceable if child.name == name.head => child }.nonEmpty => environment[Package](imported).children.collectFirst{ case child: Referenceable if child.name == name.head => child }.get
              case Import(imported,false) if environment[Module](imported).name == name.head => environment[Module](imported)
            }.orElse(where.parent.flatMap{
              case p: Package => base(p)
              case e: Environment => e.children.collectFirst{ case child: Referenceable if child.name == name.head => child }
            })
          )

        (base(contextPackage(reference)) /: name.tail) {
          case (parent, step) => for {
            parent <- parent
            child <- parent.children.collectFirst { case child: Referenceable if child.name == step => child }
          } yield child
        } getOrElse { throw new RuntimeException(s"Reference to missing module ${name.mkString(".")}") }
      }
    }
  }

  implicit class RuntimeModule(module: Module)(implicit environment: Environment) {
    def ancestors: Seq[Module] = {
      val objectClass = environment[Module]("wollok.Object")
      module +: (module match {
        case _: Mixin | `objectClass` => Nil
          //TODO .Reference[T].target:T
        case module: Class => module.mixins.flatMap { _.target.asInstanceOf[Module].ancestors } ++ module.superclass.fold(Seq(objectClass)){ _.target.asInstanceOf[Module].ancestors }
        case module: Singleton => module.mixins.flatMap { _.target.asInstanceOf[Module].ancestors } ++ module.superclass.fold(Seq(objectClass)){ _._1.target.asInstanceOf[Module].ancestors }
      })
    }

    // TODO: drop overrided
    def allMembers = module.ancestors.flatMap{ _.members : Seq[Member[_ <: Module]] }

    def lookup(message: Name, arguments: Int): Option[Method] = module.allMembers.collectFirst {
      case m: Method if m.parameters.size == arguments || m.parameters.size < arguments && m.parameters.last.hasVariableLength => m
    }

    def constructorLookup(arguments: Int): Option[Constructor] = module.allMembers.collectFirst{
      case c: Constructor if c.parameters.size == arguments || c.parameters.size < arguments && c.parameters.last.hasVariableLength => c
    }
  }

  implicit class RuntimeSuper(call: Super)(implicit environment: Environment) {
    def target = {
      def contextMethod(node: Node): Method = node match {
        case method: Method => method
        case _ => contextMethod(node.parent.get)
      }

      val cm = contextMethod(call)
      cm.parent.asInstanceOf[Module].ancestors(1).lookup(cm.name, call.arguments.size).get
    }
  }
}

// NODE NAVIGATION
//    def mapAccum[T](acum: T, acumTx: (T, Node) => T)(tx: (T, Node) => Node): (T, this.type) = {
//      val next = tx(acum, this)
//      val childAcum = acumTx(acum, next)
//      def continue[N <: Node](n: N) = n.mapAccum(childAcum, acumTx)(tx)._2
//
//      childAcum -> (next match {
//        case e: Environment => e.copy(members = e.members map continue)
//        case node: Package => node.copy(members = node.members map continue, imports = node.imports map continue)
//        case node: Program => node.copy(body = node.body map continue)
//        case node: Test => node.copy(body = node.body map continue)
//        case node: Class => node.copy(members = node.members map continue, mixins = node.mixins map continue, superclass = node.superclass map continue)
//        case node: Mixin => node.copy(members = node.members map continue)
//        case node: Singleton => node.copy(members = node.members map continue, mixins = node.mixins map continue, superclass = node.superclass map { case (name, args) => continue(name) -> (args map continue) })
//        case node: Field => node.copy(value = node.value map continue)
//        case node: Method => node.copy(parameters = node.parameters map continue, body = node.body map { _ map continue })
//        case node: Constructor => node.copy(parameters = node.parameters map continue, body = node.body map { _ map continue }, baseArguments = node.baseArguments map continue)
//        case node: Variable => node.copy(value = node.value map continue)
//        case node: Return => node.copy(value = continue(node.value))
//        case node: Assignment => node.copy(reference = continue(node.reference), value = continue(node.value))
//        case node: Send => node.copy(receiver = continue(node.receiver), arguments = node.arguments map continue)
//        case node: Super => node.copy(arguments = node.arguments map continue)
//        case node: New => node.copy(className = continue(node.className), arguments = node.arguments map continue)
//        case node: If => node.copy(condition = continue(node.condition), thenBody = node.thenBody map continue, elseBody = node.elseBody map continue)
//        case node: Throw => node.copy(continue(node.argument))
//        case node: Try => node.copy(body = node.body map continue, catches = node.catches map continue, always = node.always map continue)
//        case node: Catch => node.copy(parameter = continue(node.parameter), parameterType = node.parameterType map continue, body = node.body map continue)
//        case node @ Literal(value: Singleton) => node.copy(value = continue(value))
//        case Self | _: Literal[_] | _: LocalReference | _: FullyQualifiedReference | _: Import | _: Parameter => next
//      }).asInstanceOf[this.type]
//    }

//    def map(f: PartialFunction[Node, Node]): this.type = {
//      val fallback: PartialFunction[Node, Node] = { case n => n }
//      mapAccum[Unit]((), { case (a, _) => a }) { case (_, n) => (f orElse fallback)(n) }._2
//    }




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
// - Variables repetidas
// - LocalReferences fuera de scope
// - FullyQualifiedReferences no en el ambiente
// - Nadie tiene dos children con el mismo nombre
// - Todas las expresiones FQR son a Singletons
// - Ninguna variable o nombre se llama como una palabra reservada.
// - Override overridea y no se overridea sin override.
// - Linearización de atributos.
// - No se instancian clases abstractas
// - re-assignments to const
// - No hay super en el constructor
// - Super sin target
// - Los scopes no oclusionan: No está permitido "pisar" una variable/field/parámetro con otro