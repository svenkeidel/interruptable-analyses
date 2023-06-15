import scala.collection.mutable
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import reflect.Selectable.reflectiveSelectable

class WorklistAlgorithmSpec extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {

  def analyzeProgram1: Object {
    val task1: AnalysisTask
    val task2: AnalysisTask
    val task3: AnalysisTask
    val task4: AnalysisTask
  } = new {
    val task1 = AnalysisTask(name = "x = 0", iteration = 1, dependers = mutable.ArrayBuffer())
    val task2 = AnalysisTask(name = "while(x < 10)", iteration = 2, dependers = mutable.ArrayBuffer())
    val task3 = AnalysisTask(name = "x = x + 1", iteration = 2, dependers = mutable.ArrayBuffer())
    val task4 = AnalysisTask(name = "print(x)", iteration = 2, dependers = mutable.ArrayBuffer())

    task1.dependers += task2
    task2.dependers += task3
    task2.dependers += task4
    task3.dependers += task2
  }

  test("A worklist algorithm should process analysis tasks until nothing changes anymore") {
    val ts = analyzeProgram1
    fix(ts.task1) should be (mutable.ArrayBuffer(
      ts.task1,
      ts.task2,
      ts.task3,
      ts.task4,
      ts.task2,
      ts.task3,
      ts.task4
    ))
  }

  test("A resumable worklist algorithm should process analysis tasks until nothing changes anymore") {
    val ts = analyzeProgram1
    val iter = fixResumable(ts.task1)
    iter.next() shouldBe ts.task1
    iter.next() shouldBe ts.task2
    iter.next() shouldBe ts.task3
    iter.next() shouldBe ts.task4
    iter.next() shouldBe ts.task2
    iter.next() shouldBe ts.task3
    iter.next() shouldBe ts.task4
  }

  def analyzeProgram2: Object {
    val task1: AnalysisTask
    val task2: AnalysisTask
    val task3: AnalysisTask
    val task4: AnalysisTask
  } = new {
    val task1 = AnalysisTask(name = "x = 0", iteration = 1, dependers = mutable.ArrayBuffer())
    val task2 = AnalysisTask(name = "while(x < 10)", iteration = 2, dependers = mutable.ArrayBuffer())
    val task3 = AnalysisTask(name = "foo(x)", iteration = 2, dependers = mutable.ArrayBuffer())
    val task4 = AnalysisTask(name = "print(x)", iteration = 2, dependers = mutable.ArrayBuffer())

    task1.dependers += task2
    task2.dependers += task3
    task2.dependers += task4
    task3.dependers += task2
  }
}
