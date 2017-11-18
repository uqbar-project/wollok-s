package org.uqbar.wollok

import org.scalatest.FreeSpec
import org.scalatest.Matchers

class InterpreterTest extends FreeSpec with Matchers {
  /*
describe('Wollok interpreter', () => {

  describe('Packages', () => {

    it('should interpret packages as nested fields in a hash', () => {
      const jsEnvironment = interpret(langNatives)(link(
        Package('a')(
          Package('c')()
        ),
        Package('b')(
          Package('d')()
        )
      ))

      expect(jsEnvironment)
        .to.have.property('a')
        .and.also.to.have.nested.property('a.c')
        .and.also.to.have.property('b')
        .and.also.to.have.nested.property('b.d')
    })

  })

  describe('Classes', () => {
    it('should interpret classes as js classes', () => {
      const e = link(wre,
        Package('p')(
          Class('C')()()
        )
      );
      const jsEnvironment = interpret(langNatives)(e)

      expect(jsEnvironment)
        .to.have.nested.property('p.C').that.is.a('function')
    })

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

  describe('literals', () => {

    it('should interpret boolean literals as Booleans', () => {
      const e = link(wre, Package('p')(Singleton('s')()(Field('f', false, Literal(true)))))

      expect(interpret(langNatives)(e).p.s.f)
        .to.have.nested.property('constructor.name', '$Boolean')
        .and.also.have.property('$inner', true)
    })

    it('should interpret string literals as $String', () => {
      const e = link(wre, Package('p')(Singleton('s')()(Field('f', false, Literal('foo')))))

      expect(interpret(langNatives)(e).p.s.f)
        .to.have.nested.property('constructor.name', '$String')
        .and.also.have.property('$inner', 'foo')
    })

    it('should interpret null literals as null', () => {
      const e = link(wre, Package('p')(Singleton('s')()(Field('f', false, Literal(null)))))

      expect(interpret(langNatives)(e).p.s.f).to.be.null
    })

    it('should interpret list literals as Lists', () => {
      const e = link(wre, Package('p')(Singleton('s')()(Field('f', false, List(Literal(1), Literal(2), Literal(3))))))

      expect(interpret(langNatives)(e).p.s.f)
        .to.satisfy(list => list.size().$inner === 3)
        .and.to.satisfy(list => list.get(0).$inner === 1)
        .and.to.satisfy(list => list.get(1).$inner === 2)
        .and.to.satisfy(list => list.get(2).$inner === 3)
    })

    it('should interpret round number literals as Integers', () => {
      const e = link(wre, Package('p')(Singleton('s')()(Field('f', false, Literal(5)))))

      expect(interpret(langNatives)(e).p.s.f)
        .to.have.nested.property('constructor.name', 'Integer')
        .and.also.have.property('$inner', 5)
    })

    it('should interpret non-round number literals as Doubles', () => {
      const e = link(wre, Package('p')(Singleton('s')()(Field('f', false, Literal(5.7)))))

      expect(interpret(langNatives)(e).p.s.f)
        .to.have.nested.property('constructor.name', 'Double')
        .and.also.have.property('$inner', 5.7)
    })

    it('should interpret closures without parameters', () => {
      const e = link(wre, Package('p')(Singleton('s')()(Field('f', false, Send(Closure()(Literal(5)), 'apply')()))))

      expect(interpret(langNatives)(e).p.s.f)
        .to.have.property('$inner', 5)
    })

    it('should interpret closures with parameters', () => {
      const e = link(wre, Package('p')(Singleton('s')()(Field('f', false, Send(Closure(Parameter('p'))(Reference('p')), 'apply')(Literal(5))))))

      expect(interpret(langNatives)(e).p.s.f)
        .to.have.property('$inner', 5)
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

    describe('try-catch-always', () => {

      it('should interpret non-failing tries with catches and no always clauses to be the try body result, ignoring catches', () => {
        expectInterpretationOfExpressions(
          VariableDeclaration('x', true, Literal(0)),
          Try(Assignment(Reference('x'), Literal(7)))(Catch(Parameter('e'))(Assignment(Reference('x'), Literal(1))))(),
          Reference('x')
        ).to.have.property('$inner', 7)
      })

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


//-------------------------------------------------------------------------------------------------------------------------------
// EXPRESSIONS
//-------------------------------------------------------------------------------------------------------------------------------

// [New('Set')(List(Literal(1), Literal(2))), new Set([1, 2])],



// TODO: Super

// [If(Literal(true))(Literal(1))(Literal(2)), 1],
// [If(Literal(false))(Literal(1))(Literal(2)), 2],

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