package org.http4s

/** Internal utilities to minimize differences between scalaz and cats versions
  * without affecting user experience. */
package object compat 
  extends scalaz.syntax.ToDataOps
  with scalaz.syntax.ToTypeClassOps
  with scalaz.syntax.std.ToAllStdOps

