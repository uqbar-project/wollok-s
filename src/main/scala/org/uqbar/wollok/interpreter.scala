package org.uqbar.wollok

import org.uqbar.wollok.model._
import scala.util.{ Try => STry }

class Interpreter(environment: Environment) {
  // TODO: filter what
  def run(fqr: FullyQualifiedReference) = ???
  def runTests = ???
  def runProgram = ???
  def runExpression = ???

  type Stack[T] = List[T]
  type ExecutionResult = STry[ExecutionContext]

  case class Object(module: Module, fields: Map[Name, Object] = Map(), inner: Option[Any] = None)

  case class Frame(stack: Stack[Object], locals: Map[Name, Object], pending: Seq[Sentence], catches: Seq[Catch] = Nil, always: Seq[Sentence] = Nil)

  case class ExecutionContext(stack: Stack[Frame]) {
    def getLocal(name: Name) = stack.collectFirst { case frame if frame.locals.isDefinedAt(name) => frame.locals(name) }.get
  }

  def lookup(message: Name, arguments: Int, module: Module): Method = ???
  def superLookup(arguments: Int, superCall: Super): Method = ???
  def constructorLookup(arguments: Int, className: FullyQualifiedReference): Constructor = ???

  def execute(sentence: Sentence)(implicit context: ExecutionContext): ExecutionResult = sentence match {

    case Variable(name, _, value) => for {
      ExecutionContext(Frame(value :: rest, locals, pending, c, a) :: frames) <- execute(value getOrElse Literal(null))
    } yield ExecutionContext(Frame(rest, locals.updated(name, value), pending, c, a) :: frames)

    case Return(value) => for {
      ExecutionContext(Frame(value :: _, _, _, c, a) :: next :: frames) <- execute(value)
    } yield ExecutionContext(next.copy(stack = value :: next.stack) :: frames)

    case Assignment(reference, value) => for {
      ExecutionContext(Frame(value :: rest, locals, pending, c, a) :: frames) <- execute(value)
    } yield ExecutionContext(Frame(rest, locals.updated(reference.name, value), pending, c, a) :: frames)

    case LocalReference(name) =>
      val ExecutionContext(Frame(stack, locals, pending, c, a) :: frames) = context
      STry(ExecutionContext(Frame(context.getLocal(name) :: stack, locals, pending, c, a) :: frames))

    case fqr: FullyQualifiedReference =>
      val ExecutionContext(Frame(stack, locals, pending, c, a) :: frames) = context
      STry(ExecutionContext(Frame(Object(environment[Singleton](fqr).get) :: stack, locals, pending, c, a) :: frames))

    case Self =>
      val ExecutionContext(Frame(stack, locals, pending, c, a) :: frames) = context
      STry(ExecutionContext(Frame(context.getLocal("self") :: stack, locals, pending, c, a) :: frames))

    case Send(receiver, message, arguments) => for {
      ExecutionContext(Frame(stack, locals, pending, c, a) :: frames) <- (execute(receiver) /: arguments) { case (ctx, a) => ctx flatMap { execute(a)(_) } }
      (args, rec :: rest) = stack.splitAt(arguments.size)
      method = lookup(message, arguments.size, rec.module)
      methodBody = method.body.get //TODO: Natives
    } yield ExecutionContext(Frame(Nil, method.parameters.map(_.name).zip(args.reverse).toMap.updated("self", rec), methodBody) :: Frame(rest, locals, pending, c, a) :: frames)

    case call @ Super(arguments) => for {
      ExecutionContext(Frame(stack, locals, pending, c, a) :: frames) <- (STry(context) /: arguments) { case (ctx, a) => ctx flatMap { execute(a)(_) } }
      (args, rest) = stack.splitAt(arguments.size)
      method = superLookup(arguments.size, call)
      methodBody = method.body.get //TODO: Natives
    } yield ExecutionContext(Frame(Nil, method.parameters.map(_.name).zip(args.reverse).toMap.updated("self", locals("self")), methodBody) :: Frame(rest, locals, pending) :: frames)

    case New(className, arguments) => for {
      ExecutionContext(Frame(stack, locals, pending, c, a) :: frames) <- (STry(context) /: arguments) { case (ctx, a) => ctx flatMap { execute(a)(_) } }
      (args, rec :: rest) = stack.splitAt(arguments.size)
      constructor = constructorLookup(arguments.size, className)
      constructorBody = constructor.body.get //TODO: Recursively call parent. Should we execute non-sentences as well?
    } yield ExecutionContext(Frame(Nil, constructor.parameters.map(_.name).zip(args.reverse).toMap.updated("self", Object(environment[Class](className).get)), constructorBody ++ (Return(Self) +: Nil)) :: Frame(rest, locals, pending, c, a) :: frames)

    case If(condition, thenBody, elseBody) => for {
      ExecutionContext(Frame(res :: stack, locals, pending, c, a) :: frames) <- execute(condition)
    } yield ExecutionContext(Frame(Nil, Map(), if (res.inner == Some(true)) thenBody else elseBody) +: (Frame(stack, locals, pending, c, a) +: frames))

    case Literal(value) =>
      val instance = value match {
        case true         => Object(environment[Module]("wollok.Boolean").get, Map(), Some(true))
        case false        => Object(environment[Module]("wollok.Boolean").get, Map(), Some(false))
        case n: Int       => Object(environment[Module]("wollok.Number").get, Map(), Some(n))
        case n: Double    => Object(environment[Module]("wollok.Number").get, Map(), Some(n))
        case s: String    => Object(environment[Module]("wollok.String").get, Map(), Some(s))
        case null         => Object(environment[Module]("wollok.Null").get, Map(), Some(null))
        case o: Singleton => Object(o)
      }
      STry(context.copy(stack = context.stack.head.copy(stack = instance +: context.stack.head.stack) +: context.stack.tail))

    case Throw(argument) => for {
      ExecutionContext(fs @ (Frame(exception :: stack, locals, pending, c, a) :: frames)) <- execute(argument)
      (Frame(_, _, _, catches, always) :: rest) = fs.dropWhile { !_.catches.exists { _.parameterType.fold(true) { environment[Module](_).get == exception.module } } }
      handler: Catch = catches.find(_.parameterType.fold(true) { environment[Module](_).get == exception.module }).get
    } yield ExecutionContext(Frame(Nil, Map(handler.parameter.name -> exception), handler.body ++ always) :: frames)

    case Try(body, catches, always) => STry(context.copy(Frame(Nil, Map(), body, catches, always) +: context.stack))

  }
}