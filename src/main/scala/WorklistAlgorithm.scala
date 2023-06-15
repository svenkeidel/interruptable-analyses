import scala.collection.mutable

case class AnalysisTask(name: String, var iteration: Int, var dependers: mutable.ArrayBuffer[AnalysisTask]):
  def process =
    // Do the actual analysis work
    this.iteration -= 1

  override def toString: String =
    s"AnalysisTask($name, $iteration, ...)"

def fix(initialTask: AnalysisTask): mutable.ArrayBuffer[AnalysisTask] =
  val worklist: mutable.Queue[AnalysisTask] = mutable.Queue(initialTask)
  val processedTasks: mutable.ArrayBuffer[AnalysisTask] = mutable.ArrayBuffer()

  while(worklist.nonEmpty) {
    val task = worklist.dequeue()

    task.process
    println(s"Processed $task")
    processedTasks += task

    for(depender <- task.dependers
        if depender.iteration > 0 && ! worklist.contains(depender))
      worklist += depender

    if(task.iteration > 0 && ! worklist.contains(task))
      worklist += task
  }

  processedTasks

def fixResumable(initialTask: AnalysisTask): Iterator[AnalysisTask] =
  new Iterator[AnalysisTask] {
    val worklist: mutable.Queue[AnalysisTask] = mutable.Queue(initialTask)

    override def hasNext = worklist.nonEmpty

    override def next(): AnalysisTask =
      val task = worklist.dequeue()

      task.process
      println(s"Processed $task")

      for (depender <- task.dependers
           if depender.iteration > 0 && !worklist.contains(depender))
        worklist += depender

      if (task.iteration > 0 && !worklist.contains(task))
        worklist += task

      task
  }