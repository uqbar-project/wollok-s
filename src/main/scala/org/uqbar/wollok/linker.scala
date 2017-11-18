package org.uqbar.wollok

import org.uqbar.wollok.model._

object linker {

  def apply(packages: Package*): Environment = {
    def mergeInto(members: Seq[Member[Package]], isolated: Package): Seq[Member[Package]] = members
      .collectFirst { case p: Package if p.name == isolated.name => p }
      .fold(isolated +: members) { e =>
        members.collect {
          case `e`   => e.copy(members = mergeInto(e.members, isolated))
          case other => other
        }
      }

    Environment((Seq[Member[Package]]() /: packages)(mergeInto))
  }

}