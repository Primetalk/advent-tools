package org.primetalk.advent2018

import org.primetalk.advent.tools.GraphUtils._
import org.primetalk.advent.tools.Utils

/**
  * --- Day 7: The Sum of Its Parts ---
  *
  * You find yourself standing on a snow-covered coastline; apparently, you landed a little off course. The region is too hilly to see the North Pole from here, but you do spot some Elves that seem to be trying to unpack something that washed ashore. It's quite cold out, so you decide to risk creating a paradox by asking them for directions.
  *
  * "Oh, are you the search party?" Somehow, you can understand whatever Elves from the year 1018 speak; you assume it's Ancient Nordic Elvish. Could the device on your wrist also be a translator? "Those clothes don't look very warm; take this." They hand you a heavy coat.
  *
  * "We do need to find our way back to the North Pole, but we have higher priorities at the moment. You see, believe it or not, this box contains something that will solve all of Santa's transportation problems - at least, that's what it looks like from the pictures in the instructions." It doesn't seem like they can read whatever language it's in, but you can: "Sleigh kit. Some assembly required."
  *
  * "'Sleigh'? What a wonderful name! You must help us assemble this 'sleigh' at once!" They start excitedly pulling more parts out of the box.
  *
  * The instructions specify a series of steps and requirements about which steps must be finished before others can begin (your puzzle input). Each step is designated by a single letter. For example, suppose you have the following instructions:
  *
  * Step C must be finished before step A can begin.
  * Step C must be finished before step F can begin.
  * Step A must be finished before step B can begin.
  * Step A must be finished before step D can begin.
  * Step B must be finished before step E can begin.
  * Step D must be finished before step E can begin.
  * Step F must be finished before step E can begin.
  *
  * Visually, these requirements look like this:
  *
  *
  *   -->A--->B--
  *  /    \      \
  * C      -->D----->E
  *  \           /
  *   ---->F-----
  *
  * Your first goal is to determine the order in which the steps should be completed. If more than one step is ready, choose the step which is first alphabetically. In this example, the steps would be completed as follows:
  *
  * Only C is available, and so it is done first.
  * Next, both A and F are available. A is first alphabetically, so it is done next.
  * Then, even though F was available earlier, steps B and D are now also available, and B is the first alphabetically of the three.
  * After that, only D and F are available. E is not available because only some of its prerequisites are complete. Therefore, D is completed next.
  * F is the only choice, so it is done next.
  * Finally, E is completed.
  *
  * So, in this example, the correct order is CABDFE.
  *
  * In what order should the steps in your instructions be completed?
  *
  * Your puzzle answer was BGKDMJCNEQRSTUZWHYLPAFIVXO.
  * --- Part Two ---
  *
  * As you're about to begin construction, four of the Elves offer to help. "The sun will set soon; it'll go faster if we work together." Now, you need to account for multiple people working on steps simultaneously. If multiple steps are available, workers should still begin them in alphabetical order.
  *
  * Each step takes 60 seconds plus an amount corresponding to its letter: A=1, B=2, C=3, and so on. So, step A takes 60+1=61 seconds, while step Z takes 60+26=86 seconds. No time is required between steps.
  *
  * To simplify things for the example, however, suppose you only have help from one Elf (a total of two workers) and that each step takes 60 fewer seconds (so that step A takes 1 second and step Z takes 26 seconds). Then, using the same instructions as above, this is how each second would be spent:
  *
  * Second   Worker 1   Worker 2   Done
  * 0        C          .
  * 1        C          .
  * 2        C          .
  * 3        A          F       C
  * 4        B          F       CA
  * 5        B          F       CA
  * 6        D          F       CAB
  * 7        D          F       CAB
  * 8        D          F       CAB
  * 9        D          .       CABF
  * 10        E          .       CABFD
  * 11        E          .       CABFD
  * 12        E          .       CABFD
  * 13        E          .       CABFD
  * 14        E          .       CABFD
  * 15        .          .       CABFDE
  *
  * Each row represents one second of time. The Second column identifies how many seconds have passed as of the beginning of that second. Each worker column shows the step that worker is currently doing (or . if they are idle). The Done column shows completed steps.
  *
  * Note that the order of the steps has changed; this is because steps now take time to finish and multiple workers can begin multiple steps simultaneously.
  *
  * In this example, it would take 15 seconds for two workers to complete these steps.
  *
  * With 5 workers and the 60+ second step durations described above, how long will it take to complete all of the steps?
  *
  * Your puzzle answer was 941.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day7 extends Utils {
  lazy val inputTextFromResource : Iterator[String] =
    readResource("day7.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq //.sorted

  type Player = Int
  type TaskId = Char
  type TimeInterval = Int

  //Step R must be finished before step X can begin.
  //0123456789012345678901234567890123456789
  //0000000000111111111122222222223333333333
  def parse(line: String): (TaskId, TaskId) =
    (line.charAt(5),  line.charAt(36))

  def input: Seq[(TaskId, TaskId)] = lines.map(parse).toVector

  // QBRMGJKZSNYCWDETHULPAFIVXO
  // BQRGMJKSZCDNWYEHTULPAFIVXO
  // BQRGMJKSZCDNWYEHTULPAFIVXO
  // BGKDMJCNEQRSTUZWHYLPAFIVXO
  // BGKDMJCNEQRSTUZWHYLPAFIVXO
  def answer1: String =
    new String(topologicalSortFromEdges(input).toArray)

  // Part 2

  val timeEstimateShift: TimeInterval = 61
  def timeEstimation(c: TaskId): TimeInterval = timeEstimateShift + (c - 'A')

  require(timeEstimation('Z') == timeEstimateShift + 25)
  require(timeEstimation('A') == timeEstimateShift)

  case class State(
    time: Int,
    dependencies: Map[TaskId, Set[TaskId]],
    currentTurnOccupation: Map[Player, (TimeInterval, Option[TaskId])]
  ) {
    // various statistics about state is represented as lazy vals, because it's used in many places.
    lazy val freeWorkers: Map[Player, (TimeInterval, Option[TaskId])] =
      currentTurnOccupation.filter(_._2._1 == 0)

    lazy val tasksCompletedOnLastTurn: Seq[TaskId] =
      freeWorkers.map(_._2._2).toSeq.flatten

    lazy val freeWorkerIds: Iterable[Player] =
      freeWorkers.keys

    lazy val nextWork: Set[TaskId] = {
      val (noDependencies, hasDependencies) = dependencies.partition(_._2.isEmpty)
      if (noDependencies.isEmpty && hasDependencies.nonEmpty)
        throw new IllegalArgumentException("Cycle: " + hasDependencies.toString)
      noDependencies.keySet
    }

    lazy val inWork: Set[TaskId] =
      currentTurnOccupation
        .collect{
          case (_, (t, Some(taskId)))
            if t > 0 =>
            taskId
        }
        .toSet

    lazy val toDo: Seq[TaskId] =
      (nextWork -- inWork)
        .toSeq
        .sorted
        .take(freeWorkerIds.size) // removing as many as possible

    // separate micro-steps that produce the next intermediate step

    def advanceTime: State = {
      val timeIntervals = currentTurnOccupation.toList.map(_._2._1)
        .filter(_ > 0)

      val advanceInterval = // it was just 1 initially
        if (timeIntervals.isEmpty) 1
        else timeIntervals.min
      copy(
        time = time + advanceInterval,
        currentTurnOccupation =
          currentTurnOccupation
            .view.mapValues { case (t, c) => (math.max(0, t - advanceInterval), c) }
            .toMap
      )
    }

    def completedTasksRemoved: State = {
      if (tasksCompletedOnLastTurn.isEmpty)
        this
      else
        copy(
          dependencies = dependencies
            .view.mapValues(_ -- tasksCompletedOnLastTurn)
            .filterKeys(k => !tasksCompletedOnLastTurn.contains(k))
            .toMap
        )
    }

    def distributeAvailableWorkAmongFreeWorkers(estimate: TaskId => TimeInterval): State = {
      val timeToAssembly: Seq[(TimeInterval, Some[TaskId])] =
        toDo.map(c => (estimate(c), Some(c)))
      val work: Iterable[(Player, (TimeInterval, Some[TaskId]))] =
        freeWorkerIds.zip(timeToAssembly)
      copy(
        currentTurnOccupation =
          currentTurnOccupation ++
            freeWorkerIds.map(_ -> (0, None)).toMap ++ // removing work from workers that have just completed their work
            work
      )
    }

  }

  // Attempts:
  // 362
  // 944
  // 918
  // 928
  // 941 - didn't get for some reason.
  // 942 - plus one, incorrectly
  // 941
  def runWork(edges: Seq[(TaskId, TaskId)], workersCount: Int, estimate: TaskId => Int): Int = {
    @annotation.tailrec
    def go(
      state: State
    ): Int = {
      val state2 = state
        .advanceTime // we place time advancement here, because it is needed on every step. This is also the reason to start from -1.
        .completedTasksRemoved // And we may have some tasks completed
      println("" + state2.time + ": " + state2.currentTurnOccupation.toList.sortBy(_._1).map(_._2._2.getOrElse('.')).mkString(" "))

      if (state2.nextWork.isEmpty)// We have completed all work
        state2.time
      else
        go {
          if(state2.toDo.isEmpty)
            state2
          else
            state2.distributeAvailableWorkAmongFreeWorkers(estimate)
        }
    }

    val dependencies = convertEdgesToDirectDependenciesOnlyForTrees(edges)

    val occupations = (0 until workersCount).map(w => (w, (0, None))).toMap
    go(State(-1, dependencies, occupations))
  }

  // 941
  def answer2: Int = runWork(input, 5, timeEstimation)

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }
}

