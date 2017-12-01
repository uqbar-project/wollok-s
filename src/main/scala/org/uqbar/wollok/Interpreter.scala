package org.uqbar.wollok

import scala.util.{ Try => Result }

import org.uqbar.wollok.model._
import scala.util.Failure
import org.uqbar.wollok.model.Field
import scala.language.implicitConversions

object Interpreter {
  def apply(sentences: Seq[Sentence])(implicit environment: Environment): Result[Object] = Execution(environment)(sentences).result.map{_._1}
  def apply(node: Node)(implicit environment: Environment): Result[Object] = Execution(environment)(node).result.map{_._1}
  def start(implicit environment: Environment): Execution = Execution(environment)
}

case class Object(module: Module, fields: Map[Name, Object] = Map(), inner: Option[Any] = None)

case class Frame(locals: Map[Name, Object] = Map(), parent: Option[Frame] = None) {
  def previous = parent getOrElse {
    throw new RuntimeException("Frame stack bottom reached")
  }

  def apply(name: Name): Object = locals.getOrElse(name, previous(name))

  def updated(name: Name, value: Object): Frame =
    if (locals.isDefinedAt(name)) copy(locals.updated(name, value))
    else copy(parent = Some(previous.updated(name, value)))
}

private case class ExceptionRaised(exception: Object, context: Frame) extends RuntimeException

//══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// EXECUTION
//══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

case class Execution(environment: Environment, result: Result[(Object,Frame)] = Result(null, Frame()), history: Seq[Sentence] = Seq()) {

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // WELL KNOWN INSTANCES
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  //TODO: Hoisting: What if an object constructor references an unitialized object
  protected lazy val singletonInstances: Seq[(Id, Result[Object])] = {
    def getSingletons(node: Node): Seq[Singleton] = node match {
      case p: Package                      => p.children flatMap getSingletons
      case s: Singleton if s.name.nonEmpty => Seq(s)
      case _                               => Seq()
    }

    for (singleton <- getSingletons(environment)) yield singleton.id -> result.fold(
      {throw new RuntimeException(s"Could not initialize singletons of failed execution $this")},
      { case (_,context) => initializeSingleton(singleton)(context, environment)}
    )
  }

  protected lazy val nil = Object(environment[Class]("wollok.Null"), Map(), Some(null))

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // PUBLIC INTERFACE
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  def apply(sentences: Seq[Sentence]):Execution = (this /: sentences){ _ apply _ }
  def apply(node: Node): Execution = result.fold({_ => this},{ case (_, context) => node match {
    case node: Package => copy(Linker(environment, node), Result(nil -> context))

    case node: Module => copy(Linker(environment, Package("$interpreter", Nil, node :: Nil)), Result(nil -> context))

    case node: Sentence =>
      val nextHistory = history :+ node
      val nextEnvironment = Linker(environment, Package("$interpreter", Nil, List(
        Singleton("$instance", members = List(
          Method("history", body = Some(nextHistory))
        ))
      )))
      val nextResult = exec(node)(context, nextEnvironment)

      copy(nextEnvironment, nextResult, nextHistory)

    case _ => throw new RuntimeException(s"Can't evaluate node $node")
  }})

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // SENTENCE EXECUTION
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  //TODO: TODOS LOS USOS DE ESTO TIENEN QUE SER REEMPLAZADOS POR ALGO QUE NO PIERDA EL EFECTO COLATERAL
  protected def execAll(expressions: Seq[Expression])(implicit context: Frame, environment: Environment): Result[Seq[Object]] = (Result(Seq[Object]()) /: expressions){
    case (prev, exp) => for {
      responses <- prev
      (next, _) <- exec(exp)
    } yield responses :+ next
  }

  protected def execBlock(sentences: Seq[Sentence], locals: Seq[(Name, Object)] = Nil)(implicit context: Frame, environment: Environment): Result[(Object, Frame)] =
    (Result(nil, Frame(Map(locals: _*), Some(context))) /: sentences) {
      case (prev, sentence) => for {
        (_, ctx) <- prev
        next <- exec(sentence)(ctx, environment)
      } yield next
    }

  protected def exec(sentence: Sentence)(implicit context: Frame, environment: Environment): Result[(Object, Frame)] = sentence match {

    case Variable(name, _, value) => for {
      (value, _) <- exec(value getOrElse Literal(null))
    } yield nil -> context.copy(locals = context.locals.updated(name, value))

    case Return(value) => for {
      (value, _) <- exec(value)
    } yield value -> context.previous

    case Assignment(reference, value) => for {
      (value, _) <- exec(value)
    } yield reference.target match {
      case _: Variable => nil -> context.updated(reference.name, value)
      case _: Field =>
        val self = context("self")
        val nextSelf = self.copy(fields = self.fields.updated(reference.name, value)) //TODO: un objeto guardado en dos referencias no cambiaría su estado
        nil -> context.updated("self", nextSelf)
    }

    case lr: LocalReference => Result(lr.target match {
      case _: Field         => context("self").fields(lr.name)
      case _: Referenceable => context(lr.name)
    }, context)

    case fqr: FullyQualifiedReference => Result(Object(environment[Singleton](fqr)), context)

    case Self                         => Result(context("self"), context)

    case Send(receiver, message, arguments) => for {
      rec :: args <- execAll(receiver +: arguments)
      method = rec.module.lookup(message, arguments.size).get
      methodBody = method.body.get //TODO: Natives
      methodLocals = method.parameters.map(_.name).zip(args) :+ ("self" -> rec)
      next <- execBlock(methodBody, methodLocals)
    } yield next

    case call @ Super(arguments) => for {
      args <- execAll(arguments)
      method = call.target
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
      next <- execBlock(if (bool.inner.contains(true)) thenBody else elseBody)
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
      execBlock(body).recoverWith {
        case failure: ExceptionRaised =>
          catches
            .find{ _.parameterType.fold(true){ environment[Module](_) == failure.exception.module } }
            .map { handler => execBlock(handler.body, Seq(handler.parameter.name -> failure.exception))(failure.context, environment) }
            .getOrElse(Failure(failure))
      }.transform(
        { case (result, successCtx) => execBlock(always)(successCtx, environment).map { case (_, ctx) => result -> ctx } },
        { case failure: ExceptionRaised => execBlock(always)(failure.context, environment).flatMap { case (_, ctx) => Failure(ExceptionRaised(failure.exception, ctx)) } }
      )
  }

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // OBJECT INITIALIZATION
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  protected def initializeInstance(module: Module, arguments: Seq[Object])(implicit context: Frame, environment: Environment): Result[Object] = {
    val initialState: Result[Map[Name, Object]] = {
      val allMembers = module.allMembers
      allMembers.collect{
        case Field(name, _, Some(value)) => name -> exec(value)(Frame(), environment).map{ _._1 }
      }
    }

    for {
      state <- initialState
      instance = Object(module, state)
      constructor = module.constructorLookup(arguments.size).get
      constructorBody = constructor.body.get //TODO: Recursively call parent.
      constructorLocals = constructor.parameters.map(_.name).zip(arguments.reverse) :+ ("self" -> instance)
      (_, ctx) <- execBlock(constructorBody, constructorLocals)
    } yield ctx("self")
  }

  protected def initializeSingleton(singleton: Singleton)(implicit context: Frame, environment: Environment): Result[Object] =
    initializeInstance(singleton, singleton.superclass.map{ _._2.map{ exec(_)(Frame(), environment).map{ _._1 }.get } }.getOrElse(Nil))

  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  // IMPLICITS
  //──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

  private implicit def toTryMap[T, U](seq: Seq[(T, Result[U])]): Result[Map[T, U]] = seq.foldLeft(Result(Map[T, U]())){
    case (acum, (key, value)) =>
      for {
        acum <- acum
        value <- value
      } yield acum.updated(key, value)
  }
}