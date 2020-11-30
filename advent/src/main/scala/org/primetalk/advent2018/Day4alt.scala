package org.primetalk.advent2018

import org.primetalk.advent.tools.Utils

/**
  * --- Day 4: Repose Record ---
  *
  * You've sneaked into another supply closet - this time, it's across from the prototype suit manufacturing lab. You need to sneak inside and fix the issues with the suit, but there's a guard stationed outside the lab, so this is as close as you can safely get.
  *
  * As you search the closet for anything that might help, you discover that you're not the first person to want to sneak in. Covering the walls, someone has spent an hour starting every midnight for the past few months secretly observing this guard post! They've been writing down the ID of the one guard on duty that night - the Elves seem to have decided that one guard was enough for the overnight shift - as well as when they fall asleep or wake up while at their post (your puzzle input).
  *
  * For example, consider the following records, which have already been organized into chronological order:
  *
  * [1518-11-01 00:00] Guard #10 begins shift
  * [1518-11-01 00:05] falls asleep
  * [1518-11-01 00:25] wakes up
  * [1518-11-01 00:30] falls asleep
  * [1518-11-01 00:55] wakes up
  * [1518-11-01 23:58] Guard #99 begins shift
  * [1518-11-02 00:40] falls asleep
  * [1518-11-02 00:50] wakes up
  * [1518-11-03 00:05] Guard #10 begins shift
  * [1518-11-03 00:24] falls asleep
  * [1518-11-03 00:29] wakes up
  * [1518-11-04 00:02] Guard #99 begins shift
  * [1518-11-04 00:36] falls asleep
  * [1518-11-04 00:46] wakes up
  * [1518-11-05 00:03] Guard #99 begins shift
  * [1518-11-05 00:45] falls asleep
  * [1518-11-05 00:55] wakes up
  *
  * Timestamps are written using year-month-day hour:minute format. The guard falling asleep or waking up is always the one whose shift most recently started. Because all asleep/awake times are during the midnight hour (00:00 - 00:59), only the minute portion (00 - 59) is relevant for those events.
  *
  * Visually, these records show that the guards are asleep at these times:
  *
  * Date   ID   Minute
  * 000000000011111111112222222222333333333344444444445555555555
  * 012345678901234567890123456789012345678901234567890123456789
  * 11-01  #10  .....####################.....#########################.....
  * 11-02  #99  ........................................##########..........
  * 11-03  #10  ........................#####...............................
  * 11-04  #99  ....................................##########..............
  * 11-05  #99  .............................................##########.....
  *
  * The columns are Date, which shows the month-day portion of the relevant day; ID, which shows the guard on duty that day; and Minute, which shows the minutes during which the guard was asleep within the midnight hour. (The Minute column's header shows the minute's ten's digit in the first row and the one's digit in the second row.) Awake is shown as ., and asleep is shown as #.
  *
  * Note that guards count as asleep on the minute they fall asleep, and they count as awake on the minute they wake up. For example, because Guard #10 wakes up at 00:25 on 1518-11-01, minute 25 is marked as awake.
  *
  * If you can figure out the guard most likely to be asleep at a specific time, you might be able to trick that guard into working tonight so you can have the best chance of sneaking in. You have two strategies for choosing the best guard/minute combination.
  *
  * Strategy 1: Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?
  *
  * In the example above, Guard #10 spent the most minutes asleep, a total of 50 minutes (20+25+5), while Guard #99 only slept for a total of 30 minutes (10+10+10). Guard #10 was asleep most during minute 24 (on two days, whereas any other minute the guard was asleep was only seen on one day).
  *
  * While this example listed the entries in chronological order, your entries are in the order you found them. You'll need to organize them before they can be analyzed.
  *
  * What is the ID of the guard you chose multiplied by the minute you chose? (In the above example, the answer would be 10 * 24 = 240.)
  *
  * Your puzzle answer was 21083.
  * --- Part Two ---
  *
  * Strategy 2: Of all guards, which guard is most frequently asleep on the same minute?
  *
  * In the example above, Guard #99 spent minute 45 asleep more than any other guard or minute - three times in total. (In all other cases, any guard spent any minute asleep at most twice.)
  *
  * What is the ID of the guard you chose multiplied by the minute you chose? (In the above example, the answer would be 99 * 45 = 4455.)
  *
  * Your puzzle answer was 53024.
  */
object Day4alt extends Utils {
  lazy val inputTextFromResource : Iterator[String] =
    readResource("day4.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq.sorted

  type Id = Int
  type Min = Int
  sealed trait Event
  case class SelectGuard(id: Id) extends Event
  case class FallsAsleep(min: Min) extends Event
  case class WakesUp(min: Min) extends Event
  // [1518-07-22 23:53] Guard #1949 begins shift
  // [1518-10-09 00:21] falls asleep
  //[1518-07-30 00:14] wakes up
  //[1518-09-19 00:52] wakes up

  def parse(line: String): Event = {
    val m = "\\d+".r.findAllMatchIn(line).toList.last
    val lastInt = m.toString.toInt
    if(line.endsWith("begins shift"))
      SelectGuard(lastInt)
    else if(line.endsWith("falls asleep"))
      FallsAsleep(lastInt)
    else if(line.endsWith("wakes up"))
      WakesUp(lastInt)
    else
      throw new IllegalArgumentException("line: " + line)
  }

  lazy val events: List[Event] = lines.map(parse).toList


  def collectMinutes(events: List[Event]): Map[Id, Map[Min, Int]] = {
    def go(m: Map[Id, Map[Min, Int]], currentId: Id, lastAsleepMinute: Min, events: List[Event]): Map[Id, Map[Min, Int]] = {
      events match {
        case Nil => m
        case SelectGuard(id) :: t =>
          go(m, id, lastAsleepMinute = -1, t)
        case FallsAsleep(min) :: t =>
          go(m, currentId, lastAsleepMinute = min, t)
        case WakesUp(min) :: t =>
          val minutes = lastAsleepMinute until min
          go(
            m.updated(currentId,
              minutes.foldLeft(m.getOrElse(currentId, Map())){
                case (map, minute) =>
                  map.updated(minute, map.getOrElse(minute, 0) + 1)
              }
            ),
            currentId, lastAsleepMinute = -1, t
          )
      }
    }
    go(Map(), currentId = -1, lastAsleepMinute = -1, events)
  }

  // 21083
  def answer1: Int = {
    val m = collectMinutes(events)
    val totalMinutes = m.view.mapValues(mm => mm.values.sum).toMap
    val max = totalMinutes.values.max
    val maxId = totalMinutes.filter(_._2 == max).head._1
    val timetableForId: Map[Min, Int] = m(maxId)
    val maxCount = timetableForId.values.max
    val minuteMax = timetableForId.filter(_._2 == maxCount).head._1
    minuteMax * maxId
  }

  // 53024
  def answer2: Int = {
    val m = collectMinutes(events)
    val maxSingleMinuteCount = m.values.flatMap(_.values).max
    val (id, mmm): (Id, Map[Min, Min]) = m.collect{
      case (id1, mm) if mm.values.exists(_ == maxSingleMinuteCount) =>
        (id1, mm.filter(_._2 == maxSingleMinuteCount))
    }.head
    val min = mmm.keys.head

    min * id
  }


  def main(args: Array[String]): Unit = {
    //    println("inputTimeTable: " +
    //      inputTimeTable.sortBy(a => a.date.month*10000 + a.date.day*120 + a.time.hour*60 + a.time.min).toList.mkString("\n"))
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }
}
