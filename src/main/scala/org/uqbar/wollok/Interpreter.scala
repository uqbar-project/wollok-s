package org.uqbar.wollok

import scala.util.{ Try => Result }

import org.uqbar.wollok.model.Assignment
import org.uqbar.wollok.model.Catch
import org.uqbar.wollok.model.Class
import org.uqbar.wollok.model.Constructor
import org.uqbar.wollok.model.Environment
import org.uqbar.wollok.model.FullyQualifiedReference
import org.uqbar.wollok.model.If
import org.uqbar.wollok.model.Literal
import org.uqbar.wollok.model.LocalReference
import org.uqbar.wollok.model.Method
import org.uqbar.wollok.model.Module
import org.uqbar.wollok.model.Name
import org.uqbar.wollok.model.New
import org.uqbar.wollok.model.Return
import org.uqbar.wollok.model.Self
import org.uqbar.wollok.model.Send
import org.uqbar.wollok.model.Sentence
import org.uqbar.wollok.model.Singleton
import org.uqbar.wollok.model.Super
import org.uqbar.wollok.model.Throw
import org.uqbar.wollok.model.Try
import org.uqbar.wollok.model._
import org.uqbar.wollok.model.Expression
import scala.util.Success
import scala.annotation.tailrec
import scala.util.Failure
import org.uqbar.wollok.model.Field
import scala.language.implicitConversions

case class ExceptionRaised(exception: Object, context: List[Frame]) extends RuntimeException
case class Object(module: Module, fields: Map[Name, Object] = Map(), inner: Option[Any] = None)

case class Frame(locals: Map[Name, Object] = Map())

class Interpreter(implicit environment: Environment) {

  def eval(fqr: FullyQualifiedReference) = ???
  // TODO: filter what
  def evalTests = ???
  def evalProgram = ???
  def evalExpression(expression: Expression) = exec(expression)(Frame() :: Nil).map{ _._1 }

  implicit def toTryMap[T, U](seq: Seq[(T, Result[U])]): Result[Map[T, U]] = seq.foldLeft(Result(Map[T, U]())){
    case (acum, (key, value)) =>
      for {
        acum <- acum
        value <- value
      } yield acum.updated(key, value)
  }

  implicit class RuntimeModule(module: Module)(implicit environment: Environment) {
    lazy val ancestors: Seq[Module] = {
      val objectClass = environment[Class]("wollok.Object")
      module +: (module match {
        case _: Mixin | `objectClass` => Nil
        case module: Class            => module.mixins.flatMap{ environment[Mixin](_).ancestors } ++ module.superclass.map{ environment[Class](_) }.getOrElse(objectClass).ancestors
        case module: Singleton        => module.mixins.flatMap{ environment[Mixin](_).ancestors } ++ module.superclass.map{ case (fqr, _) => environment[Class](fqr) }.getOrElse(objectClass).ancestors
      })
    }

    // TODO: drop overrided
    lazy val allMembers = module.ancestors.flatMap{ _.members: Seq[Member[_ <: Module]] }
  }

  def lookup(message: Name, arguments: Int, module: Module): Method = ???
  def superLookup(arguments: Int, superCall: Super): Method = ???
  def constructorLookup(arguments: Int, module: Module): Constructor = module.allMembers.collectFirst{
    case c: Constructor if c.parameters.size == arguments || c.parameters.size < arguments && c.parameters.last.hasVariableLength => c
  }.get

  def getLocal(name: Name)(implicit context: List[Frame]): Object = context.find(_.locals.isDefinedAt(name)).map(_.locals(name)).get
  def updateLocal(name: Name, value: Object)(implicit context: List[Frame]): List[Frame] = {
    val (init, head :: tail) = context.span(!_.locals.isDefinedAt(name))
    init ::: head.copy(locals = head.locals.updated(name, value)) :: tail
  }

  def initializeInstance(module: Module, arguments: Seq[Object])(implicit context: List[Frame]): Result[Object] = {
    val initialState: Result[Map[Name, Object]] = module.allMembers.collect{ case field @ Field(name, _, Some(value)) => name -> evalExpression(value) }

    for {
      state <- initialState
      instance = Object(module, state)
      constructor = constructorLookup(arguments.size, instance.module)
      constructorBody = constructor.body.get //TODO: Recursively call parent.
      constructorLocals = Map("self" -> instance) ++ Map(constructor.parameters.map(_.name).zip(arguments.reverse): _*)
      (_, ctx) <- exec(constructorBody)(Frame(locals = constructorLocals) :: context)
    } yield getLocal("self")(ctx)
  }

  def initializeSingleton(singleton: Singleton): Result[Object] = {
    initializeInstance(singleton, singleton.superclass.map{ _._2.map{ evalExpression(_).get } }.getOrElse(Nil))(Nil)
  }

  //TODO: Hoisting: What if an object constructor references an unitialized object
  lazy val singletonInstances = {
    def getSingletons(node: Node): Seq[Singleton] = node match {
      case p: Package                      => p.children.flatMap{ getSingletons(_) }
      case s: Singleton if s.name.nonEmpty => Seq(s)
      case _                               => Seq()
    }

    val initializedInstances: Result[Map[Id, Object]] = getSingletons(environment).map{ singleton =>
      singleton.id -> initializeSingleton(singleton)
    }

    initializedInstances
  }

  lazy val nil = Object(environment[Class]("wollok.Null"))

  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
  // EXECUTION
  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

  def execAll(expressions: Seq[Expression])(implicit context: List[Frame]): Result[Seq[Object]] = (Result(Seq[Object]()) /: expressions){
    case (prev, exp) => for {
      responses <- prev
      (next, _) <- exec(exp)
    } yield responses :+ next
  }

  def exec(sentences: Seq[Sentence])(implicit context: List[Frame]): Result[(Object, List[Frame])] = (Result(nil, context) /: sentences) {
    case (prev, sentence) => for {
      (_, ctx) <- prev
      next <- exec(sentence)(ctx)
    } yield next
  }

  def exec(sentence: Sentence)(implicit context: List[Frame]): Result[(Object, List[Frame])] = sentence match {

    case Variable(name, _, value) => for {
      (value, _) <- exec(value getOrElse Literal(null))
    } yield nil -> (context.head.copy(locals = context.head.locals + (name -> value)) :: context.tail)

    case Return(value) => for {
      (value, _) <- exec(value)
    } yield value -> context.tail

    case Assignment(reference, value) => for {
      (value, _) <- exec(value)
    } yield reference.target match {
      case f: Variable => nil -> updateLocal(reference.name, value)
      case f: Field =>
        val self = getLocal("self")
        val nextSelf = self.copy(fields = self.fields.updated(reference.name, value)) //TODO: un objeto guardado en dos referencias cambia su estado?
        nil -> updateLocal("self", nextSelf)
    }

    case LocalReference(name)         => Result(getLocal(name), context)

    case fqr: FullyQualifiedReference => Result(Object(environment[Singleton](fqr)), context)

    case Self                         => Result(getLocal("self"), context)

    case Send(receiver, message, arguments) => for {
      rec :: args <- execAll(receiver +: arguments)
      method = lookup(message, arguments.size, rec.module)
      methodBody = method.body.get //TODO: Natives
      methodLocals = method.parameters.map(_.name).zip(args).toMap + ("self" -> rec)
      next <- exec(methodBody)(Frame(methodLocals) :: context)
    } yield next

    case call @ Super(arguments) => for {
      args <- execAll(arguments)
      method = superLookup(arguments.size, call)
      methodBody = method.body.get //TODO: Natives
      methodLocals = method.parameters.map(_.name).zip(args).toMap
      next <- exec(methodBody)(Frame(locals = methodLocals) :: context)
    } yield next

    case New(className, arguments) => for {
      args <- execAll(arguments)
      instance <- initializeInstance(environment[Class](className), args)
    } yield instance -> context

    case If(condition, thenBody, elseBody) => for {
      (bool, _) <- exec(condition)
      next <- exec(if (bool.inner.exists{ _ == true }) thenBody else elseBody)(Frame() :: context)
    } yield next

    case Literal(value) =>
      val instance = value match {
        case _: Boolean   => Result(Object(environment[Module]("wollok.Boolean"), Map(), Some(value)))
        case _: Int       => Result(Object(environment[Module]("wollok.Integer"), Map(), Some(value)))
        case _: Double    => Result(Object(environment[Module]("wollok.Double"), Map(), Some(value)))
        case _: String    => Result(Object(environment[Module]("wollok.String"), Map(), Some(value)))
        case null         => Result(Object(environment[Module]("wollok.Null"), Map(), Some(value)))
        case o: Singleton => initializeSingleton(o)
      }
      instance map { _ -> context }

    case Throw(argument) => for {
      (exception, _) <- exec(argument)
      error <- Failure(ExceptionRaised(exception, context))
    } yield error

    case Try(body, catches, always) =>
      val handled = exec(body)(Frame() :: context) match {
        case fail @ Failure(ExceptionRaised(exception, Frame(failedLocals) :: failedContext)) =>
          val handler = catches.find{ _.parameterType.fold(true){ environment[Module](_) == exception.module } }
          handler.fold(fail: Result[(Object, List[Frame])]) {
            case Catch(parameter, _, catchBody) => exec(catchBody)(Frame(failedLocals + (parameter.name -> exception)) :: failedContext)
          }
        case other => other
      }

      for {
        (result, frames) <- handled
        (_, ctx) <- exec(always.toList)(Frame() :: frames)
      } yield result -> ctx
  }

}