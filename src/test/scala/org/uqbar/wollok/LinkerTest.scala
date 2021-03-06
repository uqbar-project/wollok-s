package org.uqbar.wollok

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.uqbar.wollok.model._

class LinkerTest extends FreeSpec with Matchers {

  "Wollok linker" - {

    "parent" in {
      val m = Method("m")
      val c = Class("C", members = m :: Nil)
      val q = Package("q", members = c :: Nil)
      val p = Package("p", members = q :: Nil)
      implicit val environment = Linker(p)

      environment.parent should be(None)
      p.parent should be(Some(environment))
      q.parent should be(Some(p))
      c.parent should be(Some(q))
      m.parent should be(Some(c))
    }

    val objectClass = Class("Object")
    val wre = Package("wollok", members= Seq(objectClass))

    "scope" - {

      "each node should be linked with it's scope" in {
        val m = Mixin("M")
        val q = Package("q", members = m :: Nil)
        val c = Class("C")
        val p = Package("p", members = c :: q :: Nil)
        implicit val environment = Linker(wre, p)

        p.scope should be (Map("wollok" -> wre, "Object" -> objectClass, "p" -> p))
        c.scope should be (Map("wollok" -> wre, "Object" -> objectClass,"p" -> p, "C" -> c, "q" -> q))
        q.scope should be(Map("wollok" -> wre, "Object" -> objectClass,"p" -> p, "C" -> c, "q" -> q))
        m.scope should be(Map("wollok" -> wre, "Object" -> objectClass,"p" -> p, "C" -> c, "q" -> q, "M" -> m))
      }

      "non-visible definitions should not be included on scope" in {
        val m = Mixin("M")
        val c = Class("C")
        val r = Package("r", members = m :: Nil)
        val q = Package("q", members = c :: Nil)
        val p = Package("p", members = q :: r :: Nil)
        implicit val environment = Linker(wre, p)

        p.scope should equal (Map("wollok" -> wre, "Object" -> objectClass,"p" -> p))
        q.scope should be(Map("wollok" -> wre, "Object" -> objectClass,"p" -> p, "q" -> q, "r" -> r))
        r.scope should be(Map("wollok" -> wre, "Object" -> objectClass,"p" -> p, "q" -> q, "r" -> r))
        c.scope should be(Map("wollok" -> wre, "Object" -> objectClass,"p" -> p, "q" -> q, "r" -> r, "C" -> c))
        m.scope should be(Map("wollok" -> wre, "Object" -> objectClass,"p" -> p, "q" -> q, "r" -> r, "M" -> m))
      }

      "outer scope entries should be overrided by inner ones" in {
        val m1cr = LocalReference("x")
        val m1cp = Parameter("x")
        val m1c = Closure(m1cp :: Nil, m1cr :: Nil)
        val m1p = Parameter("x")
        val m1 = Method("m1", parameters = m1p :: Nil, body = Some(m1c :: Nil))
        val m2r = LocalReference("x")
        val m2v = Variable("x", false)
        val m2 = Method("m2", body = Some(m2v :: m2r :: Nil))
        val f = Field("x", false)
        val s = Singleton("x", members = f :: m1 :: m2 :: Nil)
        val p = Package("x", members = s :: Nil)
        implicit val environment = Linker(wre, p)

        p.scope.apply("x") should be (p)
        s.scope.apply("x") should be (s)
        f.scope.apply("x") should be (f)
        m1.scope.apply("x") should be (f)
        m1p.scope.apply("x") should be (m1p)
        m1c.scope.apply("x") should be (m1p)
        m1cp.scope.apply("x") should be (m1cp)
        m1cr.scope.apply("x") should be (m1cp)
        m2.scope.apply("x") should be (f)
        m2v.scope.apply("x") should be (m2v)
        m2r.scope.apply("x") should be (m2v)
      }

    }

    "target" - {

      "local references should target the scope element they reference" in {
        val r = LocalReference("a")
        val p = Parameter("a")
        val m = Method("m", parameters = p :: Nil, body = Some(r :: Nil))
        val c = Class("C", members = m :: Nil)
        val q = Package("q", members = c :: Nil)
        implicit val environment = Linker(wre, q)

        r.target should be(p)
      }

      "fully qualified references should target the global element they reference" in {
        val r = FullyQualifiedReference("q" :: "S" :: Nil)
        val m = Method("m", body = Some(r :: Nil))
        val s = Singleton("S", members = m :: Nil)
        val q = Package("q", members = s :: Nil)
        implicit val environment = Linker(wre, q)

        r.scope.get("q") should be(Some(q))
        //r.target should be(s)
      }

      "fully qualified references should target the relative element they reference" in {
        val r = FullyQualifiedReference("S" :: Nil)
        val m = Method("m", body = Some(r :: Nil))
        val s = Singleton("S", members = m :: Nil)
        val q = Package("q", members = s :: Nil)
        implicit val environment = Linker(wre, q)

        r.target should be(s)
      }

    }

    "ancestors" - {

      "singleton literals should have ancestors" in {
        val singletonLiteral = Singleton("")
        implicit val environment = Linker(wre, Package("p", members = Seq(
          Singleton("S", members=Seq(
            Field("f", isReadOnly = false, Some(Literal(singletonLiteral)))
          ))
        )))

        singletonLiteral.ancestors should be(singletonLiteral :: environment[Class]("wollok.Object") :: Nil)
      }

    }

  }

}