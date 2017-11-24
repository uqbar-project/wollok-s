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
import sun.awt.X11.Separator
import sun.awt.X11.Separator

case class ExceptionRaised(exception: Object, context: Frame) extends RuntimeException
case class Object(module: Module, fields: Map[Name, Object] = Map(), inner: Option[Any] = None)

case class Frame(locals: Map[Name, Object] = Map(), parent: Option[Frame] = None) {
  def previous = parent getOrElse {
    throw new RuntimeException("Frame stack bottom reached")
  }

  def apply(name: Name): Object = locals.get(name) getOrElse previous(name)

  def updated(name: Name, value: Object): Frame =
    if (locals.isDefinedAt(name)) copy(locals.updated(name, value))
    else copy(parent = Some(previous.updated(name, value)))
}

class InteractiveInterpreter(initialEnvironment: Environment) extends Interpreter {
  protected implicit var currentContext = Frame()
  protected implicit var environment = Linker(
    initialEnvironment,
    Package("$interpreter", Nil, List(
      Singleton("$instance", members = List(
        Method("history", body = Some(Nil))
      ))
    ))
  )

  def stack = currentContext

  def eval(sentences: Seq[Sentence]) = {
    val currentHistory = environment[Singleton]("$interpreter.$instance").members.collectFirst{ case Method("history", _, _, _, Some(body)) => body }
    environment = Linker(initialEnvironment, Package("$interpreter", Nil, List(
      Singleton("$instance", members = List(
        Method("history", body = currentHistory map { _ ++ sentences })
      ))
    )))
    val result = execBlock(sentences)
    result foreach { case (_, ctx) => currentContext = ctx }
    result map { _._1 }
  }

  def eval(node: Node) = node match {
    case node: Package =>
      environment = Linker(environment, node)
      Result(nil)
    case node: Module =>
      environment = Linker(environment, Package("$interpreter", Nil, node :: Nil))
      Result(nil)
    case node: Sentence =>
      val currentHistory = environment[Singleton]("$interpreter.$instance").members.collectFirst{ case Method("history", _, _, _, Some(body)) => body }
      environment = Linker(initialEnvironment, Package("$interpreter", Nil, List(
        Singleton("$instance", members = List(
          Method("history", body = currentHistory map { _ :+ node })
        ))
      )))
      val result = exec(node)
      result foreach { case (_, ctx) => currentContext = ctx }
      result map { _._1 }
    case _ => throw new RuntimeException(s"Can't evaluate node $node")
  }

}

trait Interpreter {

  implicit protected def environment: Environment

  def evalExpression(expression: Expression) = exec(expression)(Frame()).map{ _._1 }

  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
  // AUXILIARS
  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

  protected def lookup(message: Name, arguments: Int, module: Module): Method = ???
  protected def superLookup(arguments: Int, superCall: Super): Method = ???
  protected def constructorLookup(arguments: Int, module: Module): Constructor = module.allMembers.collectFirst{
    case c: Constructor if c.parameters.size == arguments || c.parameters.size < arguments && c.parameters.last.hasVariableLength => c
  }.get

  protected def initializeInstance(module: Module, arguments: Seq[Object])(implicit context: Frame): Result[Object] = {
    val initialState: Result[Map[Name, Object]] = module.allMembers.collect{ case field @ Field(name, _, Some(value)) => name -> evalExpression(value) }

    for {
      state <- initialState
      instance = Object(module, state)
      constructor = constructorLookup(arguments.size, instance.module)
      constructorBody = constructor.body.get //TODO: Recursively call parent.
      constructorLocals = constructor.parameters.map(_.name).zip(arguments.reverse) :+ ("self" -> instance)
      (_, ctx) <- execBlock(constructorBody, constructorLocals)
    } yield ctx("self")
  }

  protected def initializeSingleton(singleton: Singleton): Result[Object] = {
    initializeInstance(singleton, singleton.superclass.map{ _._2.map{ evalExpression(_).get } }.getOrElse(Nil))(Frame())
  }

  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
  // WELL KNOWN INSTANCES
  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

  //TODO: Hoisting: What if an object constructor references an unitialized object
  protected lazy val singletonInstances = {
    def getSingletons(node: Node): Seq[Singleton] = node match {
      case p: Package                      => p.children.flatMap{ getSingletons(_) }
      case s: Singleton if s.name.nonEmpty => Seq(s)
      case _                               => Seq()
    }

    for (singleton <- getSingletons(environment)) yield singleton.id -> initializeSingleton(singleton)
  }

  lazy val nil = Object(environment[Class]("wollok.Null"), Map(), Some(null))

  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
  // EXECUTION
  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

  protected def execAll(expressions: Seq[Expression])(implicit context: Frame): Result[Seq[Object]] = (Result(Seq[Object]()) /: expressions){
    case (prev, exp) => for {
      responses <- prev
      (next, _) <- exec(exp)
    } yield responses :+ next
  }

  protected def execBlock(sentences: Seq[Sentence], locals: Seq[(Name, Object)] = Nil)(implicit context: Frame): Result[(Object, Frame)] =
    (Result(nil, Frame(Map(locals: _*), Some(context))) /: sentences) {
      case (prev, sentence) => for {
        (_, ctx) <- prev
        next <- exec(sentence)(ctx)
      } yield next
    }

  protected def exec(sentence: Sentence)(implicit context: Frame): Result[(Object, Frame)] = sentence match {

    case Variable(name, _, value) => for {
      (value, _) <- exec(value getOrElse Literal(null))
    } yield nil -> context.copy(locals = context.locals.updated(name, value))

    case Return(value) => for {
      (value, _) <- exec(value)
    } yield value -> context.previous

    case Assignment(reference, value) => for {
      (value, _) <- exec(value)
    } yield reference.target match {
      case f: Variable => nil -> context.updated(reference.name, value)
      case f: Field =>
        val self = context("self")
        val nextSelf = self.copy(fields = self.fields.updated(reference.name, value)) //TODO: un objeto guardado en dos referencias cambia su estado?
        nil -> context.updated("self", nextSelf)
    }

    case LocalReference(name)         => Result(context(name), context)

    case fqr: FullyQualifiedReference => Result(Object(environment[Singleton](fqr)), context)

    case Self                         => Result(context("self"), context)

    case Send(receiver, message, arguments) => for {
      rec :: args <- execAll(receiver +: arguments)
      method = lookup(message, arguments.size, rec.module)
      methodBody = method.body.get //TODO: Natives
      methodLocals = method.parameters.map(_.name).zip(args) :+ ("self" -> rec)
      next <- execBlock(methodBody, methodLocals)
    } yield next

    case call @ Super(arguments) => for {
      args <- execAll(arguments)
      method = superLookup(arguments.size, call)
      methodBody = method.body.get //TODO: Natives
      methodLocals = method.parameters.map(_.name).zip(args)
      next <- execBlock(methodBody, methodLocals)
    } yield next

    case New(className, arguments) => for {
      args <- execAll(arguments)
      instance <- initializeInstance(environment[Class](className), args)
    } yield instance -> context

    case If(condition, thenBody, elseBody) => for {
      (bool, _) <- exec(condition)
      next <- execBlock(if (bool.inner.exists{ _ == true }) thenBody else elseBody)
    } yield next

    case Literal(value) =>
      val instance = value match {
        case _: Boolean   => Result(Object(environment[Module]("wollok.Boolean"), Map(), Some(value)))
        case _: Int       => Result(Object(environment[Module]("wollok.Integer"), Map(), Some(value)))
        case _: Double    => Result(Object(environment[Module]("wollok.Double"), Map(), Some(value)))
        case _: String    => Result(Object(environment[Module]("wollok.String"), Map(), Some(value)))
        case null         => Result(nil)
        case o: Singleton => initializeSingleton(o)
      }
      instance map { _ -> context }

    case Throw(argument) => for {
      (exception, _) <- exec(argument)
      error <- Failure(ExceptionRaised(exception, context))
    } yield error

    case Try(body, catches, always) =>
      val handled = execBlock(body).recoverWith {
        case failure: ExceptionRaised =>
          catches
            .find{ _.parameterType.fold(true){ environment[Module](_) == failure.exception.module } }
            .map { handler => execBlock(handler.body, Seq(handler.parameter.name -> failure.exception))(failure.context) }
            .getOrElse(Failure(failure))
      }

      handled.transform(
        { case (result, successCtx) => for ((_, ctx) <- execBlock(always)(successCtx)) yield result -> ctx },
        { case failure: ExceptionRaised => execBlock(always)(failure.context).flatMap { case (_, ctx) => Failure(ExceptionRaised(failure.exception, ctx)) } }
      )
  }

  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
  // IMPLICITS
  //══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

  implicit def toTryMap[T, U](seq: Seq[(T, Result[U])]): Result[Map[T, U]] = seq.foldLeft(Result(Map[T, U]())){
    case (acum, (key, value)) =>
      for {
        acum <- acum
        value <- value
      } yield acum.updated(key, value)
  }

  implicit class RuntimeModule(module: Module)(implicit environment: Environment) {
    def ancestors: Seq[Module] = {
      val objectClass = environment[Class]("wollok.Object")
      module +: (module match {
        case _: Mixin | `objectClass` => Nil
        case module: Class            => module.mixins.flatMap{ environment[Mixin](_).ancestors } ++ module.superclass.map{ environment[Class](_) }.getOrElse(objectClass).ancestors
        case module: Singleton        => module.mixins.flatMap{ environment[Mixin](_).ancestors } ++ module.superclass.map{ case (fqr, _) => environment[Class](fqr) }.getOrElse(objectClass).ancestors
      })
    }

    // TODO: drop overrided
    def allMembers = module.ancestors.flatMap{ _.members: Seq[Member[_ <: Module]] }
  }

}