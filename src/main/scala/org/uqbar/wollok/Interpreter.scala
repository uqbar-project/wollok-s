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
import org.uqbar.wollok.model.Variable
import org.uqbar.wollok.model.Expression
import scala.util.Success
import scala.annotation.tailrec
import scala.util.Failure

class Interpreter(environment: Environment) {

  type Stack[T] = List[T]

  def eval(fqr: FullyQualifiedReference) = ???
  // TODO: filter what
  def evalTests = ???
  def evalProgram = ???

  def evalExpression(expression: Expression) = executeAll(Result(Frame(pending = expression :: Nil) :: Nil)).map{ _.head.stack.head }

  case class Object(module: Module, fields: Map[Name, Object] = Map(), inner: Option[Any] = None)

  case class Frame(
    stack:   Stack[Object]     = Nil,
    locals:  Map[Name, Object] = Map(),
    pending: Seq[Sentence]     = Nil,
    catches: Seq[Catch]        = Nil,
    always:  Seq[Sentence]     = Nil
  )

  def getLocal(context: Stack[Frame], name: Name) = context.collectFirst { case frame if frame.locals.isDefinedAt(name) => frame.locals(name) }.get
  def lookup(message: Name, arguments: Int, module: Module): Method = ???
  def superLookup(arguments: Int, superCall: Super): Method = ???
  def constructorLookup(arguments: Int, className: FullyQualifiedReference): Constructor = ???

  @tailrec
  private def executeAll(context: Result[Stack[Frame]]): Result[Stack[Frame]] = context match {
    case failure: Failure[_] => failure
    case Success(frames @ Frame(result :: _, _, Nil, _, _) :: Nil) => Result(frames)
    case Success(frames) => executeAll(execute(frames))
  }

  protected def execute(context: List[Frame]): Result[Stack[Frame]] = context match {
    case Nil => Result { Nil.head }

    case (frame @ Frame(_, _, Variable(name, _, value) :: pending, _, _)) :: frames => for {
      Frame(value :: rest, locals, pending, c, a) :: frames <- execute(frame.copy(pending = value.getOrElse(Literal(null)) :: pending) :: Nil)
    } yield Frame(rest, locals.updated(name, value), pending, c, a) :: frames

    case Frame(stack, locals, Return(value) :: pending, catches, always) :: frames => for {
      Frame(value :: _, _, _, c, a) :: next :: frames <- execute(Frame(stack, locals, value :: pending, catches, always) :: Nil)
    } yield next.copy(stack = value :: next.stack) :: frames

    case Frame(stack, locals, Assignment(reference, value) :: pending, catches, always) :: frames => for {
      Frame(value :: rest, locals, pending, c, a) :: frames <- execute(Frame(stack, locals, value :: pending, catches, always) :: Nil)
    } yield Frame(rest, locals.updated(reference.name, value), pending, c, a) :: frames

    case Frame(stack, locals, LocalReference(name) :: pending, catches, always) :: frames =>
      Result(Frame(getLocal(context, name) :: stack, locals, pending, catches, always) :: frames)

    case Frame(stack, locals, (fqr: FullyQualifiedReference) :: pending, catches, always) :: frames =>
      Result(Frame(Object(environment[Singleton](fqr).get) :: stack, locals, pending, catches, always) :: frames)

    case Frame(stack, locals, Self :: pending, catches, always) :: frames =>
      Result(Frame(getLocal(context, "self") :: stack, locals, pending, catches, always) :: frames)

    case (frame @ Frame(stack, locals, Send(receiver, message, arguments) :: pending, catches, always)) :: frames => for {
      Frame(stack, locals, pending, c, a) :: frames <- execute(frame.copy(pending = receiver +: arguments) :: Nil)
      (args, rec :: rest) = stack.splitAt(arguments.size)
      method = lookup(message, arguments.size, rec.module)
      methodBody = method.body.get //TODO: Natives
    } yield Frame(Nil, method.parameters.map(_.name).zip(args.reverse).toMap.updated("self", rec), methodBody) :: Frame(rest, locals, pending, c, a) :: frames

    case (frame @ Frame(stack, locals, (call @ Super(arguments)) :: pending, catches, always)) :: frames => for {
      Frame(stack, locals, pending, c, a) :: frames <- execute(frame.copy(pending = arguments) :: Nil)
      (args, rest) = stack.splitAt(arguments.size)
      method = superLookup(arguments.size, call)
      methodBody = method.body.get //TODO: Natives
    } yield Frame(Nil, method.parameters.map(_.name).zip(args.reverse).toMap.updated("self", locals("self")), methodBody) :: Frame(rest, locals, pending) :: frames

    case (frame @ Frame(stack, locals, New(className, arguments) :: pending, catches, always)) :: frames => for {
      Frame(stack, locals, pending, c, a) :: frames <- execute(frame.copy(pending = arguments) :: Nil)
      (args, rec :: rest) = stack.splitAt(arguments.size)
      constructor = constructorLookup(arguments.size, className)
      constructorBody = constructor.body.get //TODO: Recursively call parent. Should we execute non-sentences as well?
    } yield Frame(Nil, constructor.parameters.map(_.name).zip(args.reverse).toMap.updated("self", Object(environment[Class](className).get)), constructorBody ++ (Return(Self) +: Nil)) :: Frame(rest, locals, pending, c, a) :: frames

    case (frame @ Frame(stack, locals, If(condition, thenBody, elseBody) :: pending, catches, always)) :: frames => for {
      Frame(res :: stack, locals, _, c, a) :: frames <- execute(frame.copy(pending = condition :: Nil) :: Nil)
    } yield Frame(Nil, Map(), if (res.inner == Some(true)) thenBody else elseBody) +: (Frame(stack, locals, pending, c, a) +: frames)

    case (frame @ Frame(stack, locals, Literal(value) :: pending, catches, always)) :: frames =>
      val instance = value match {
        case true         => Object(environment[Module]("wollok.Boolean").get, Map(), Some(true))
        case false        => Object(environment[Module]("wollok.Boolean").get, Map(), Some(false))
        case n: Int       => Object(environment[Module]("wollok.Number").get, Map(), Some(n))
        case n: Double    => Object(environment[Module]("wollok.Number").get, Map(), Some(n))
        case s: String    => Object(environment[Module]("wollok.String").get, Map(), Some(s))
        case null         => Object(environment[Module]("wollok.Null").get, Map(), Some(null))
        case o: Singleton => Object(o)
      }
      Result(frame.copy(stack = instance :: frame.stack) :: frames)

    case (frame @ Frame(stack, locals, Throw(argument) :: pending, catches, always)) :: frames => for {
      fs @ (Frame(exception :: stack, locals, pending, c, a) :: frames) <- execute(frame.copy(pending = argument :: pending) :: Nil)
      (Frame(_, _, _, catches, always) :: rest) = fs.dropWhile { !_.catches.exists { _.parameterType.fold(true) { environment[Module](_).get == exception.module } } }
      handler: Catch = catches.find(_.parameterType.fold(true) { environment[Module](_).get == exception.module }).get
    } yield Frame(Nil, Map(handler.parameter.name -> exception), handler.body ++ always) :: frames

    case frames @ Frame(stack, locals, Try(body, catches, always) :: pending, _, _) :: _ => Result(Frame(Nil, Map(), body, catches, always) :: frames)
  }
}