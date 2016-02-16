import scala.language.higherKinds

/**
  * User: Oleg
  * Date: 17-Feb-16
  * Time: 01:36
  */
package object bow {
  type >=:[W[_], M[_]] = Distribute[W, M]
}
