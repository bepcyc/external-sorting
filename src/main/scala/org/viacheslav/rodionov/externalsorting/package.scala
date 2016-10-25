package org.viacheslav.rodionov

import scala.io.Source._

package object externalsorting {
  trait ExternalAccess {
    // closes input stream correctly
    def fileLines(path: String) = fromFile(path).getLines
  }
}
