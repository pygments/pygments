import // This is incorrect Scala but can still be highlighted correctly
import a.{x => y} // Test comment
import a.{x => } // This is incorrect Scala but can still be highlighted correctly
import a.{x => `test-name`}
import a.given
import a.{given a}
import a.{x, y}
import a._
import a.x
import a.x.y.z
import java.io.{File, IOException, FileNotFoundException}
import java.io.File
import scala.math.{given Ordering[Int]}
import scala.math.{given Ordering[?]}
import a.givenSomething
import givenPackage
