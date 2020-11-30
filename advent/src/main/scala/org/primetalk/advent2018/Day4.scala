package org.primetalk.advent2018

import org.primetalk.advent.tools.Utils

import scala.annotation.tailrec

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
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day4 extends Utils {
  lazy val inputTextFromResource : Iterator[String] =
    readResource("day4.txt")

  lazy val lines: Seq[String] =
    inputTextFromResource.toSeq

  case class Date(month: Int, day: Int)
  case class Time(hour: Int, min: Int)
  sealed trait Record {
    val date: Date
    val time: Time
  }
  case class BeginsShift(date: Date, time: Time, id: Int) extends Record
  sealed trait SleepingRecord extends Record
  case class FallsAsleep(date: Date, time: Time) extends SleepingRecord
  case class WakesUp(date: Date, time: Time) extends SleepingRecord

  // [1518-07-22 23:53] Guard #1949 begins shift
  // [1518-10-09 00:21] falls asleep
  //[1518-07-30 00:14] wakes up
  //[1518-09-19 00:52] wakes up
  //[1518-07-22 00:02] Guard #1129 begins shift
  val regex = "\\[\\d{4}-(\\d\\d)-(\\d\\d) (\\d\\d):(\\d\\d)\\] (.*)$".r("month", "day", "hour", "min", "text")
  val shiftRegex = "Guard #(\\d+) begins shift".r("id")
  def parse(line: String): Record = {
    val Some(m) = regex.findFirstMatchIn(line)

    val hour = m.group("hour").toInt
    val min = m.group("min").toInt
    val time = Time(
      if(hour == 23) 0 else hour,
      if(hour == 23) min - 60 else min
    )

    val month = m.group("month").toInt
    val day = m.group("day").toInt
    val date =
      if(hour == 23) {
        if(
          (day == 30 && Set(4, 6).contains(month)) ||
            (day == 28 && Set(2).contains(month)) ||
            (day == 31 && Set(7, 8).contains(month))
        )
          Date(month + 1, 1) // TODO: max day for that month.
        else
          Date(month, day + 1)
      }
      else
        Date(month, day)

    m.group("text") match {

      case "falls asleep" => FallsAsleep(date, time)
      case "wakes up" => WakesUp(date, time)
      case guard =>
        val Some(m2) = shiftRegex.findFirstMatchIn(guard)
        val id = m2.group("id").toInt
        BeginsShift(date, time, id)
    }
  }

  lazy val inputTimeTable: Seq[Record] = lines.map(parse)

  def guardShifts(records: Seq[Record]): Map[Date, Int] =
    records
      .collect{ case BeginsShift(date, _, id) => (date, id)}
      .toMap

  def dateTimes(records: Seq[Record]): Map[Date, List[SleepingRecord]] =
    records
      .groupBy(_.date)
      .map{
        case (date, seq) =>
          (date,
            seq
              .collect{ case a: SleepingRecord => a}
              .sortBy(t => t.time.hour * 60 + t.time.min)
              .toList
          )
      }

  def minutesAsleep(sleepingRecords: List[SleepingRecord]): Int = {
    @tailrec
    def go(sleeping: Boolean, count: Int, previousPos: Int, records: List[SleepingRecord]): Int =
      records match {
        case Nil    =>
          if(sleeping)
            count + 60 - previousPos
          else
            count
        case FallsAsleep(_, Time(_, min)) :: t =>
          go(sleeping = true, count, min, t)
        case WakesUp(_, Time(_, min)) :: t =>
          go(sleeping = false, count + (min - previousPos), min, t)
    }

    go(sleeping = false, 0,0,sleepingRecords)
  }

  def minutesAsleep2(sleepingRecords: List[SleepingRecord]): Set[Int] = {
    @tailrec
    def go(sleeping: Boolean, minutes: Set[Int], previousPos: Int, records: List[SleepingRecord]): Set[Int] =
      records match {
        case Nil    =>
          if(sleeping)
            minutes ++ (previousPos until 60).toSet
          else
            minutes
        case FallsAsleep(_, Time(_, min)) :: t =>
          go(sleeping = true, minutes, min, t)
        case WakesUp(_, Time(_, min)) :: t =>
          go(sleeping = false, minutes ++ (previousPos until min).toSet, min, t)
    }


    go(sleeping = false, Set(),-60,sleepingRecords)
  }
  def minutesAsleep3(sleepingRecords: List[SleepingRecord]): Int = minutesAsleep2(sleepingRecords).size

  def guardSleeping(records: Seq[Record]): Map[Int, Int] = {
    val shifts: Map[Date, Int] = guardShifts(records)
    val dateTi: Map[Date, List[SleepingRecord]] = dateTimes(records)

    val dateToMinutes: Map[Date, Int] = dateTi.view.mapValues(minutesAsleep3).toMap
    dateToMinutes.toList
      .map{ case (date, mins) =>
        (shifts(date), mins) // TODO: id from previous date
    }.groupBy(_._1).map{ case (id, minList) => (id, minList.map(_._2).sum)}

  }
  lazy val shifts: Map[Date, Int] = guardShifts(inputTimeTable)
  lazy val dateTi: Map[Date, List[SleepingRecord]] = dateTimes(inputTimeTable)

  lazy val dateToMinutes: Map[Date, Int] = dateTi.view.mapValues(minutesAsleep).toMap


  lazy val guardToDatesToSleepingMinutes: Map[Int, Map[Date,Set[Int]]] =
    shifts.toList
      .groupBy(_._2)
      .map{ case (id, lst) =>
        (id,
          lst
            .map{
              case (date, _) => (date, minutesAsleep2(dateTi(date)))
            }
            .toMap
        )
      }
  def answer1: Int = {
    val shifts: Map[Date, Int] = guardShifts(inputTimeTable)
    val dateTi: Map[Date, List[SleepingRecord]] = dateTimes(inputTimeTable)

    val dateToMinutes: Map[Date, Int] = dateTi.view.mapValues(minutesAsleep).toMap
//    dateToMinutes
//      .map{ case (date, mins) =>
//        (shifts(date), mins) // TODO: id from previous date
//      }
//      .toList
    val (id, mins2) = guardSleeping(inputTimeTable).maxBy(_._2)
    println("id " + id + "; sleep= " + mins2)

    val datesForId: Set[Date] = shifts.toList.filter(_._2 == id).map(_._1).toSet
    val dateToRecordsForId: Map[Date, List[SleepingRecord]] =
      dateTi.view.filterKeys(datesForId.contains).toMap
    val filtered = dateToRecordsForId.toList.sortBy(a => a._1.month*31 + a._1.day)

    println(filtered.map{ case (date, lst) => "" + date + ":" + minutesAsleep2(lst).toSeq.sorted}.mkString("\n"))

    val eachMinutes: Map[Date, Set[Int]] = dateToRecordsForId.view.mapValues(minutesAsleep2).toMap
    val minutesWithCounts = (0 until 60).map{ min => min->eachMinutes.count(_._2.contains(min))}
    println(minutesWithCounts.mkString("\n"))
    val min = minutesWithCounts.maxBy(_._2)
    println("max " + min)
    id*min._1
  }
  // Part 2
  def answer2: Int = {
//    dateToMinutes
//      .map{ case (date, mins) =>
//        (shifts(date), mins) // TODO: id from previous date
//      }
//      .toList
//    val (id, mins2) = guardSleeping(inputTimeTable).maxBy(_._2)
//    println("id " + id + "; sleep= " + mins2)
//
//    val datesForId: Set[Date] = shifts.toList.filter(_._2 == id).map(_._1).toSet
//    val dateToRecordsForId: Map[Date, List[SleepingRecord]] =
//      dateTi.filterKeys(datesForId.contains)
//    val filtered = dateToRecordsForId.toList.sortBy(a => a._1.month*31 + a._1.day)
//
//    println(filtered.map{ case (date, lst) => "" + date + ":" + minutesAsleep2(lst).toSeq.sorted}.mkString("\n"))

    val minutesWithCountsPerId: Map[Int, Seq[(Int, Int)]] =
      guardToDatesToSleepingMinutes
        .map{
          case (id2, eachMinutes) =>
          val minutesWithCounts: Seq[(Int, Int)] = (0 until 60).map{ min => min->eachMinutes.count(_._2.contains(min))}
            (id2, minutesWithCounts)
        }
    val id = minutesWithCountsPerId.maxBy(i => i._2.maxBy(_._2)._2 )._1
    val (min, count) = minutesWithCountsPerId(id).maxBy(_._2)

    println("id " + id)
    println("min " + min)
    println("count " + count)
    id*min
  }

  def main(args: Array[String]): Unit = {
//    println("inputTimeTable: " +
//      inputTimeTable.sortBy(a => a.date.month*10000 + a.date.day*120 + a.time.hour*60 + a.time.min).toList.mkString("\n"))
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }
}
