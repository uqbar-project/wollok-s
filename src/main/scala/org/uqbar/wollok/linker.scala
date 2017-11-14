package org.uqbar.wollok

import org.uqbar.wollok.model._

object linker {

  implicit class ExtendedSeq[T](self: Seq[T]) {
    def replace(old: T, next: T) = self.collect {
      case `old` => next
      case other => other
    }
  }

  def apply(packages: Package*): Environment = {

    def mergeInto(isolated: Package, members: Seq[Member[Package]]): Seq[Member[Package]] = members
      .collectFirst { case p @ Package(isolated.name, _, _, _) => p }
      .fold(isolated +: members) { e => members.replace(e, e.copy(members = mergeInto(isolated, e.members))) }

    val environment = (Environment() /: packages) { (env, pack) => Environment(mergeInto(pack, env.members)) }

    environment
  }

  protected def linkPaths(environment: Environment) = {

    def linkPath[N <: Node](nodePath: Path[N])(node: Node): N = {
      def linkAllPaths[P <: Node, C <: Node](parent: P)(stepName: String, step: P => Seq[C])(implicit parentPath: Path[P]) =
        step(parent).zipWithIndex.map { case (node, i) => linkPath((parentPath / s"$stepName/$i") { step(_)(i) })(node) }

      (node match {
        case node: Package =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path    = Some(path),
            imports = linkAllPaths(node)("imports", _.imports),
            members = linkAllPaths(node)("members", _.members)
          )

        case node: Class =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path       = Some(path),
            members    = linkAllPaths(node)("members", _.members),
            superclass = node.superclass map linkPath((path / "superclass") { _.superclass.get }),
            mixins     = linkAllPaths(node)("mixins", _.mixins)
          )

        case node: Mixin =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path    = Some(path),
            members = linkAllPaths(node)("members", _.members)
          )

        case node: Singleton =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path       = Some(path),
            members    = linkAllPaths(node)("members", _.members),
            superclass = node.superclass map { case ((className, classArguments)) => linkPath((path / "superclass") { _.superclass.get._1 })(className) -> linkAllPaths(node)("superclassArguments", _.superclass.get._2) },
            mixins     = linkAllPaths(node)("mixins", _.mixins)
          )

        case node: Program =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path      = Some(path),
            body = linkAllPaths(node)("body", _.body)
          )

        case node: Test =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path      = Some(path),
            body = linkAllPaths(node)("body", _.body)
          )

        case node: Field =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path = Some(path),
            value = node.value map linkPath((path / "value") { _.value.get }),
          )

        case node: Method =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path = Some(path),
            parameters = linkAllPaths(node)("parameters", _.parameters),
            body = node.body map {_ => linkAllPaths(node)("body", _.body.get) }
          )

        case node: Constructor =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path = Some(path),
            parameters = linkAllPaths(node)("parameters", _.parameters),
            body = node.body map {_ => linkAllPaths(node)("body", _.body.get) },
            baseArguments = linkAllPaths(node)("baseArguments", _.baseArguments)
          )

        case node: Variable =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path = Some(path),
            value = node.value map linkPath((path / "value") { _.value.get })
          )

        case node: Return =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path = Some(path),
            value = linkPath((path/"value"){_.value})(node)
          )

        case node: Assignment =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path = Some(path),
            reference =  linkPath((path/"reference"){_.reference})(node),
            value = linkPath((path/"value"){_.value})(node)
          )

        case node: Closure =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path = Some(path),
            parameters = linkAllPaths(node)("parameters", _.parameters),
            body = linkAllPaths(node)("body", _.body)
          )

        case node: Send =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path = Some(path),
            receiver = linkPath((path/"receiver"){_.receiver})(node),
            arguments = linkAllPaths(node)("arguments", _.arguments)
          )

        case node: Super =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path = Some(path),
            arguments = linkAllPaths(node)("arguments", _.arguments)
          )

        case node: New =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path = Some(path),
            className = linkPath((path/"receiver"){_.className})(node),
            arguments = linkAllPaths(node)("arguments", _.arguments)
          )

        case node: Throw =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path = Some(path),
            argument = linkPath((path/"argument"){_.argument})(node),
          )

        case node: If =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path = Some(path),
            condition = linkPath((path/"condition"){_.condition})(node),
            thenBody = linkAllPaths(node)("thenBody", _.thenBody),
            elseBody = linkAllPaths(node)("elseBody", _.elseBody)
          )

        case node: Try =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path = Some(path),
            body = linkAllPaths(node)("body", _.body),
            catches = linkAllPaths(node)("catches", _.catches),
            always = linkAllPaths(node)("always", _.always),
          )

        case node: Catch =>
          implicit val path = nodePath.asInstanceOf[Path[node.type]]
          node.copy(
            path = Some(path),
            parameter = linkPath((path/"parameter"){_.parameter})(node),
            parameterType = node.parameterType map {_ => linkPath((path/"parameterType"){_.parameterType.get})(node)},
            body = linkAllPaths(node)("body", _.body)
          )

//        case node@Literal(value: Singleton, _) =>
//          val path = nodePath.asInstanceOf[Path[node.type]]
//          implicit val valuePath = (path / "value") {_.value.asInstanceOf[Singleton]}
//          node.copy(
//            path = Some(path),
//            value = value.copy(
//            path       = Some(valuePath),
//            members    = linkAllPaths(value)("members", _.members),
//            superclass = value.superclass map { case ((className, classArguments)) => linkPath((valuePath / "superclass") { _.superclass.get._1 })(className) -> linkAllPaths(value)("superclassArguments", _.superclass.get._2) },
//            mixins     = linkAllPaths(value)("mixins", _.mixins)
//          )
//          )

      }).asInstanceOf[N]
    }

  }
}

////===============================================================================================================================
//// LINKER
////===============================================================================================================================
//
////TODO: Overload to receive a current-environment for incremental building.
//export default (...packages) => {
//  const environment = packages.reduce(mergeInto, Package('')())
//
//  // TODO: Perhaps Txs like addDefaultConstructor should not be done here. Are there any more?
//  const completedEnvironment = addDefaultConstructor(environment)
//
//  //TODO: Manejar casos de error
//  const response = [
//    linkPath(),
//    linkScope,
//    linkReferences
//  ].reduce((env, step) => step(env), completedEnvironment)
//
//  return response
//}
//
////-------------------------------------------------------------------------------------------------------------------------------
//// PATH LINKING
////-------------------------------------------------------------------------------------------------------------------------------
//

//
////-------------------------------------------------------------------------------------------------------------------------------
//// SCOPE LINKING
////-------------------------------------------------------------------------------------------------------------------------------
//
//const linkScope = environment => {
//  const addToScope = selector => node => selector(node).reduce((scope, elem) => (
//    { ...scope, [elem.name]: elem.path }
//  ), {})
//
//  const importedItems = environment => importRef => (importRef.name.split('.').slice(-1)[0] === '*'
//    ? importRef.path.parent()(environment).elements
//    : importRef)
//
//  const scopeWithin = environment => path => {
//    const scopeContributions = match({
//      [Package]: node => {
//        const wre = environment.elements.find(_ => _.name === 'wollok')
//        return addToScope(_ => [
//          ...wre ? wre.elements : [],
//          ..._.elements,
//          ..._.imports.reduce((a, i) => [...a, ...importedItems(environment)(i)])
//        ])(node)
//      },
//      [Module]: addToScope(_ => _.members.filter(m => m.is(Field))),
//      [[Constructor, Method, Closure]]: addToScope(_ => _.parameters),
//      [Block]: addToScope(_ => _.sentences.filter(s => s.is(VariableDeclaration))),
//      [Catch]: addToScope(_ => [_.variable]),
//      [Node]: () => {}
//    })(path(environment))
//
//    return path.isRoot() ? scopeContributions : { ...scopeWithin(environment)(path.parent()), ...scopeContributions }
//  }
//
//  return flatMap({
//    //TODO: Perhaps we should use a node Root or Environment instead of Package, so we don't have to check?
//    [Package]: node => (node.path.isRoot() ? node : node.copy({ scope: scopeWithin(environment)(node.path.parent()) })),
//    [Node]: node => node.copy({ scope: scopeWithin(environment)(node.path.parent()) })
//  })(environment)
//}
//
////-------------------------------------------------------------------------------------------------------------------------------
//// REFERENCE LINKING
////-------------------------------------------------------------------------------------------------------------------------------
//
//const linkReferences = environment => flatMap({
//  [Reference]: node => node.copy({
//    target: () => {
//      const steps = node.name.split('.')
//      //TODO: Add QualifiedName node to avoid the if?
//      const target = steps.length > 1
//        ? steps.reduce((target, name) => target(environment).elements.find(_ => _.name === name).path, Path())
//        : node.scope[node.name]
//      if (!target || !target(environment)) throw new ReferenceError(`Reference ${node.name} is not in scope`)
//      return target
//    }
//  }),
//  [Node]: node => node
//})(environment)