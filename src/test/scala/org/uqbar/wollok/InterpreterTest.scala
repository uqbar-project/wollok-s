package org.uqbar.wollok

import org.scalatest.FreeSpec
import org.scalatest.Matchers

import model._
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult
import org.uqbar.wollok._
import scala.language.implicitConversions

class InterpreterTest extends FreeSpec with InterpreterMatchers {

  val wre = Package("wollok", Nil, List(
    Class("Integer"),
    Class("Double"),
    Class("Boolean"),
    Class("String"),
    Class("Null"),
    Class("Object", members = Constructor() :: Nil)
  ))

  "Wollok interpreter" - {

    implicit val environment = Environment(wre :: Nil)

    "literal" - {

      "should interpret boolean literals as Wollok Booleans" in {
        Literal(true) should beInterpretedTo (true)
      }

      "should interpret string literals as Wollok Strings" in {
        Literal("foo") should beInterpretedTo ("foo")
      }

      "should interpret null literals as Wollok Null" in {
        Literal(null) should beInterpretedTo (toObject(null))
      }

      "should interpret round number literals as Wollok Integers" in {
        Literal(1) should beInterpretedTo (1)
      }

      "should interpret non-round number literals as Wollok Doubles" in {
        Literal(1.0) should beInterpretedTo (1.0)
      }

      "should interpret object literals as initialized instances" in {

        implicit val environment = Environment(List(
          wre,
          Package("p", Nil, List(
            Class("C", members = List(
              Field("g", false, Some(Literal(2))),
              Field("h", true, None),
              Constructor(Parameter("_h") :: Nil, body = Some(List(
                Assignment(LocalReference("h"), LocalReference("_h"))
              )))
            ))
          ))
        ))

        val singleton = Singleton("", Some("p.C", Literal(3) :: Nil), Nil, Field("f", true, Some(Literal(1))) :: Nil)

        Literal(singleton) should beInterpretedTo (Object(singleton, Map("f" -> 1, "g" -> 2, "h" -> 3), None))
      }

    }

    "if" - {

      "with truthy condition should evaluate it's then-body without evaluating it's else-body" in {
        val expression = If(Literal(true), Literal(1) :: Nil, Throw(New("Error")) :: Nil)
        expression should beInterpretedTo (1)
      }

      "with falsy condition should evaluate it's else-body without evaluating it's then-body" in {
        val expression = If(Literal(false), Throw(New("Error")) :: Nil, Literal(1) :: Nil)
        expression should beInterpretedTo (1)
      }

    }

    "assignment" - {

      "should change mutable local reference's value" in {

        Seq(
          Variable("x", false, Some(Literal(0))),
          Assignment(LocalReference("x"), Literal(1)),
          LocalReference("x")
        ) should beSequentiallyInterpretedTo (1)

      }
    }

    "throw" - {

      "should interpret non-failing tries with catches and no always clauses to be the try body result, ignoring catches" in {
        Seq(
          Variable("x", false, Some(Literal(0))),
          Try(List(
            Assignment(LocalReference("x"), Literal(7))
          ), List(
            Catch(Parameter("e"), None, List(
              Assignment(LocalReference("x"), Literal(1))
            ))
          )),
          LocalReference("x")
        ) should beSequentiallyInterpretedTo (1)
      }

      /*
      it('should interpret non-failing tries with catches and always clauses to be the always body result after executing the try body, ignoring catches', () => {
        expectInterpretationOfExpressions(
          VariableDeclaration('x', true, Literal(0)),
          Try(Assignment(Reference('x'), Literal(7)))(Catch(Parameter('e'))(Assignment(Reference('x'), Literal(1))))(Assignment(Reference('x'), Send(Reference('x'), '+')(Literal(1)))),
          Reference('x')
        ).to.have.property('$inner', 8)
      })

      it('should interpret non-failing tries with no catches and always clauses to be the always body result after executing the try body', () => {
        expectInterpretationOfExpressions(
          VariableDeclaration('x', true, Literal(0)),
          Try(Assignment(Reference('x'), Literal(7)))()(Assignment(Reference('x'), Send(Reference('x'), '+')(Literal(1)))),
          Reference('x')
        ).to.have.property('$inner', 8)
      })

      it('should interpret failing tries with matching catch and no always clauses to be the catch result, ignoring try body after error', () => {
        expectInterpretationOfExpressions(
          VariableDeclaration('x', true, Literal(0)),
          Try(Throw(New(Reference('Exception'))()), Assignment(Reference('x'), Literal(7)))(Catch(Parameter('e'))(Assignment(Reference('x'), Send(Reference('x'), '+')(Literal(1)))))(),
          Reference('x')
        ).to.have.property('$inner', 1)
      })

      it('should interpret failing tries with matching catch and always clauses to be the catch result, ignoring try body after error but after executing the always', () => {
        expectInterpretationOfExpressions(
          VariableDeclaration('x', true, Literal(0)),
          Try(Throw(New(Reference('Exception'))()), Assignment(Reference('x'), Literal(7)))(Catch(Parameter('e'))(Assignment(Reference('x'), Send(Reference('x'), '*')(Literal(2)))))(Assignment(Reference('x'), Send(Reference('x'), '+')(Literal(1)))),
          Reference('x')
        ).to.have.property('$inner', 2)
      })

      it('should interpret failing tries with no catches and always clauses to propagate the error, ignoring try body after error but after executing the always', () => {
        expectInterpretationOfExpressions(
          VariableDeclaration('x', true, Literal(0)),
          Try(
            Try(
              Throw(New(Reference('Exception'))()), Assignment(Reference('x'), Literal(7))
            )(
            )(
              Assignment(Reference('x'), Send(Reference('x'), '+')(Literal(1)))
            ),
          )(
            Catch(Parameter('e'))(
              Assignment(Reference('x'), Send(Reference('x'), '+')(Literal(1)))
            )
          )(
          ),
          Reference('x')
        ).to.have.property('$inner', 2)
      })

      it('should interpret failing tries with no matching catches to propagate the error, ignoring try body after error', () => {
        expectInterpretationOfExpressions(
          VariableDeclaration('x', true, Literal(0)),
          Try(
            Try(
              Throw(New(Reference('Exception'))()),
              Assignment(Reference('x'), Literal(7))
            )(
              Catch(Parameter('e'), Reference('StackOverflowException'))(
                Assignment(Reference('x'), Literal(5))
              )
            )(
            )
          )(
            Catch(Parameter('e'))(
              Assignment(Reference('x'), Send(Reference('x'), '+')(Literal(1)))
            )
          )(
          ),
          Reference('x')
        ).to.have.property('$inner', 1)
      })

      it('should interpret failing tries with multiple matching catches to the result of the first one, ignoring try body after error', () => {
        expectInterpretationOfExpressions(
          VariableDeclaration('x'),
          Try(Throw(New(Reference('Exception'))()), Assignment(Reference('x'), Literal(7)))(
            Catch(Parameter('e'), Reference('StackOverflowException'))(Assignment(Reference('x'), Literal(5))),
            Catch(Parameter('e'), Reference('Exception'))(Assignment(Reference('x'), Literal(2))),
            Catch(Parameter('e'))(Assignment(Reference('x'), Literal(6)))
          )(),
          Reference('x')
        ).to.have.property('$inner', 2)
      })

       * */
    }

    /*

  describe('Classes', () => {

    it('should provide instances with their methods', () => {
      const e = link(wre,
        Package('p')(
          Class('C')()(
            Method('m')(Parameter('a'))(Reference('a'))
          )
        )
      )

      const { p: { C } } = interpret(langNatives)(e)
      const instance = new C()

      expect(instance).to.respondTo('m')
      expect(instance.m(5)).to.equal(5)
    })

    it('should provide instances with their fields', () => {
      const e = link(wre,
        Package('p')(
          Class('C')()(
            Field('f', true, Literal(7))
          )
        )
      )

      const { p: { C } } = interpret(langNatives)(e)
      const instance = new C()

      expect(instance).to.have.property('f')
      expect(instance.f.$inner).to.equal(7)
    })

    it('should provide instances with their superclass methods', () => {
      const e = link(wre,
        Package('p')(
          Class('C')()(
            Method('m')(Parameter('a'))(Reference('a'))
          ),
          Class('D')(Reference('C'))()
        )
      )

      const { p: { D } } = interpret(langNatives)(e)
      const instance = new D()

      expect(instance).to.respondTo('m')
      expect(instance.m(5)).to.equal(5)
    })

    it('should provide instances with their superclass fields', () => {
      const e = link(wre,
        Package('p')(
          Class('C')()(
            Field('f', true, Literal(7))
          ),
          Class('D')(Reference('C'))()
        )
      )

      const { p: { D } } = interpret(langNatives)(e)
      const instance = new D()

      expect(instance).to.have.property('f')
      expect(instance.f.$inner).to.equal(7)
    })

    it('should override methods', () => {
      const e = link(wre,
        Package('p')(
          Class('C')()(
            Method('m')(Parameter('a'))(Reference('a')),
            Method('m')()(Literal(5))
          ),
          Class('D')(Reference('C'))(
            Method('m', true)()(Literal(7))
          )
        )
      )

      const { p: { D } } = interpret(langNatives)(e)
      const instance = new D()

      expect(instance).to.respondTo('m')
      expect(instance.m().$inner).to.equal(7)
      expect(instance.m(5)).to.equal(5)
    })
  })

  describe('Mixins', () => {

    it('should interpret mixins as js functions', () => {
      const e = link(wre,
        Package('p')(
          Mixin('M')()
        )
      )

      const jsEnvironment = interpret(langNatives)(e)

      expect(jsEnvironment)
        .to.have.nested.property('p.M').that.is.a('function')
    })

    it('should provide instances with their methods', () => {
      const e = link(wre,
        Package('p')(
          Mixin('M')(
            Method('m')(Parameter('a'))(Reference('a'))
          ),
          Class('C')(undefined, Reference('M'))()
        )
      )
      const { p: { C } } = interpret(langNatives)(e)
      const instance = new C()

      expect(instance).to.respondTo('m')
      expect(instance.m(5)).to.equal(5)
    })

    it('should provide instances with their fields', () => {
      const e = link(wre,
        Package('p')(
          Mixin('M')(
            Field('f', true, Literal(7))
          ),
          Class('C')(undefined, Reference('M'))()
        )
      )


      const { p: { C } } = interpret(langNatives)(e)
      const instance = new C()

      expect(instance).to.have.property('f')
      expect(instance.f.$inner).to.equal(7)
    })

    it('should override methods', () => {
      const e = link(wre,
        Package('p')(
          Class('C')(undefined, Reference('M'), Reference('N'))(),
          Mixin('M')(
            Method('m')(Parameter('a'))(Reference('a')),
            Method('m')()(Literal(5))
          ),
          Mixin('N')(
            Method('m', true)()(Literal(7))
          )
        )
      )

      const { p: { C } } = interpret(langNatives)(e);
      const instance = new C()

      expect(instance).to.respondTo('m')
      expect(instance.m().$inner).to.equal(7)
      expect(instance.m(5)).to.equal(5)
    })
  })

  describe('Singletons', () => {

    it('should interpret singletons as js objects', () => {
      const e = link(wre,
        Package('p')(
          Singleton('s')()()
        )
      )

      const jsEnvironment = interpret(langNatives)(e)

      expect(jsEnvironment)
        .to.have.nested.property('p.s').that.is.an('object')
    })

    it('should respond to their methods', () => {
      const e = link(wre,
        Package('p')(
          Singleton('s')()(
            Method('m')(Parameter('a'))(Reference('a'))
          )
        )
      )

      const { p: { s: instance } } = interpret(langNatives)(e)

      expect(instance).to.respondTo('m')
      expect(instance.m(5)).to.equal(5)
    })

    it('should contain their fields', () => {
      const e = link(wre,
        Package('p')(
          Singleton('s')()(
            Field('f', true, Literal(7))
          )
        )
      )

      const { p: { s: instance } } = interpret(langNatives)(e)

      expect(instance).to.have.property('f')
      expect(instance.f.$inner).to.equal(7)
    })

    it('should provide instances with their superclass methods', () => {
      const e = link(wre,
        Package('p')(
          Class('C')()(
            Method('m')(Parameter('a'))(Reference('a'))
          ),
          Singleton('s')(Reference('C'))()
        )
      )

      const { p: { s: instance } } = interpret(langNatives)(e)

      expect(instance).to.respondTo('m')
      expect(instance.m(5)).to.equal(5)
    })

    it('should provide instances with their superclass fields', () => {
      const e = link(wre,
        Package('p')(
          Class('C')()(
            Field('f', true, Literal(7))
          ),
          Singleton('s')(Reference('C'))()
        )
      )

      const { p: { s: instance } } = interpret(langNatives)(e)

      expect(instance).to.have.property('f')
      expect(instance.f.$inner).to.equal(7)
    })

    it('should override methods', () => {
      const e = link(wre,
        Package('p')(
          Class('C')()(
            Method('m')(Parameter('a'))(Reference('a')),
            Method('m')()(Literal(5))
          ),
          Singleton('s')(Reference('C'))(
            Method('m', true)()(Literal(7))
          )
        )
      )

      const { p: { s: instance } } = interpret(langNatives)(e)

      expect(instance).to.respondTo('m')
      expect(instance.m().$inner).to.equal(7)
      expect(instance.m(5)).to.equal(5)
    })

  })

  describe('expressions', () => {

    function expectInterpretationOfExpressions(...expressions) {
      const e = link(wre, Package('p')(Singleton('s')()(Method('m')()(...expressions))))
      return expect(interpret(langNatives)(e).p.s.m())
    }

    describe('references', () => {

      it('should interpret declared references', () => {
        expectInterpretationOfExpressions(
          VariableDeclaration('x', true, Literal(5)),
          Reference('x')
        ).to.have.property('$inner', 5)
      })

    })

  })

})


// describe.skip('Wollok interpreter', () => {

// describe('sentences', () => {

//   it('should interpret assignment of mutable variables', () => {
//     expectInterpretationOf(
//       VariableDeclaration(Reference('x'), true, Literal(1)),
//       Assignment(Reference('x'), Send(Reference('x'), '+')(Literal(4))),
//       Reference('x')
//     ).to.have.property('$inner', 5)
//   })

//   it('should interpret assignment of immutable variables as an error', () => {
//     expectErrorOnInterpretationOf(
//       VariableDeclaration(Reference('x'), false, Literal(1)),
//       Assignment(Reference('x'), Literal(5)),
//       Reference('x')
//     ).to.be(TypeError, 'Assignment to constant variable.')
//   })

// })

// [New('Set')(List(Literal(1), Literal(2))), new Set([1, 2])],

// TODO: Super

// [Try(Literal(1))(Catch(Parameter('e'))(Literal(2)))(), 1],
// [Try(Literal(1))()(Literal(3)), 3],
// [Try(Throw(Literal('woops')))(Catch(Parameter('e'))(Literal(2)))(), 2],

// [Send(ClosureNode()(
//   Return(Literal(2)),
//   Literal(1)
// ), 'call')(), 2],
// [Send(ClosureNode()(
//   Literal(1),
//   Return(Literal(2))
// ), 'call')(), 2],
//
// })
 */
  }

  implicit def toObject(value: Any)(implicit environment: Environment): Object = value match {
    case value: Boolean => Object(environment[Module]("wollok.Boolean"), Map(), Some(value))
    case value: Int     => Object(environment[Module]("wollok.Integer"), Map(), Some(value))
    case value: Double  => Object(environment[Module]("wollok.Double"), Map(), Some(value))
    case value: String  => Object(environment[Module]("wollok.String"), Map(), Some(value))
    case null           => Object(environment[Module]("wollok.Null"), Map(), Some(value))
  }
}

//══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
// MATCHERS
//══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

trait InterpreterMatchers extends Matchers {

  case class beInterpretedTo(expected: Object)(implicit environment: Environment) extends Matcher[Expression] {
    def apply(target: Expression) = {
      val result = new Interpreter()(environment).evalExpression(target)

      MatchResult(
        result.isSuccess && result.get == expected,
        if (result.isSuccess) s"Interpreted result ${result.get} did not equal $expected" else s"execution failed: ${result}",
        if (result.isSuccess) s"Interpreted result ${result.get} was equal to $expected" else s"execution failed: ${result}"
      )
    }
  }

  case class beSequentiallyInterpretedTo(expected: Object)(implicit environment: Environment) extends Matcher[Seq[Sentence]] {
    def apply(target: Seq[Sentence]) = {
      val result = new Interpreter()(environment).exec(target)(Frame() :: Nil)

      MatchResult(
        result.isSuccess && result.get._1 == expected,
        if (result.isSuccess) s"Interpreted result ${result.get._1} did not equal $expected" else s"execution failed: ${result}",
        if (result.isSuccess) s"Interpreted result ${result.get._1} was equal to $expected" else s"execution failed: ${result}"
      )
    }
  }

}