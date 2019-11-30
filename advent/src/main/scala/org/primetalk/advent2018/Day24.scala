package org.primetalk.advent2018

import org.primetalk.advent.tools.SequenceUtils._
import org.primetalk.advent.tools.CollectionUtils._
import org.primetalk.advent.tools.Utils

import scala.util.Try

object Day24Parser  {
  import fastparse._
  import NoWhitespace._
  import ParsingUtils._
  sealed trait AttackKind
  case object Fire extends AttackKind
  case object Radiation extends AttackKind
  case object Bludgeoning extends AttackKind
  case object Cold extends AttackKind
  case object Slashing extends AttackKind

  type Id = Int

  case class GroupOfUnits(unitsCount: Int, hitPoints: Int,
    attackKind: AttackKind, attackDamage: Int, immuneTo: Seq[AttackKind], weakTo: Seq[AttackKind],
    initiative: Int
  ) {
    def effectivePower: Int = unitsCount * attackDamage
    def id: Id = initiative
  }

  case class AutonomousGroup(id: Id, armyKind: ArmyKind, groupOfUnits: GroupOfUnits) {
    def applyDamage(d: Int): AutonomousGroup = {
      val removeUnits = d / groupOfUnits.hitPoints
      val restUnits = math.max(0, groupOfUnits.unitsCount - removeUnits)
      this.copy(groupOfUnits = groupOfUnits.copy(unitsCount = restUnits))
    }
  }

  val attackKinds = List(
    "fire" -> Fire,
    "radiation" -> Radiation,
    "bludgeoning" -> Bludgeoning,
    "cold" -> Cold,
    "slashing" -> Slashing,
  )

  def attackKind[_ : P]: P[AttackKind] =
  P(
    "fire".parseAs(Fire) |
    "radiation".parseAs(Radiation) |
    "bludgeoning".parseAs(Bludgeoning) |
    "cold".parseAs(Cold) |
    "slashing".parseAs(Slashing)
  )

  sealed trait InfoKind
  case object Immunes extends InfoKind // (immuneTo: Seq[AttackKind])
  case object Weaks extends InfoKind

  def listOfImmunesAndWeaks[_ : P]: P[Seq[(InfoKind, Seq[AttackKind])]] =
    P("("
      ~ (
        ("immune to ".parseAs(Immunes)| "weak to ".parseAs(Weaks))
          ~ attackKind.rep(min = 1, sep = ", ")
      ).rep(sep = "; ")
      ~ ") "
    )

  def groupOfUnits[_ : P]: P[GroupOfUnits] = P(positiveNumber ~ " units each with " ~ positiveNumber ~ " hit points "
    ~ (listOfImmunesAndWeaks| "".!.map(_ => Seq())) ~ "with an attack that does " ~ positiveNumber ~ " " ~ attackKind ~
    " damage at initiative " ~ positiveNumber
  ).map{
    case (unitsCount: Int, hitPoints: Int, listOfImmunesAndWeaks,
      attackDamage: Int, attackKind: AttackKind,
      initiative: Int) =>
      val imm   = listOfImmunesAndWeaks.collectFirst { case (Immunes, seq) => seq }.getOrElse(Seq())
      val weaks = listOfImmunesAndWeaks.collectFirst { case (Weaks, seq) => seq }.getOrElse(Seq())
      GroupOfUnits(unitsCount, hitPoints, attackKind, attackDamage, imm, weaks, initiative)
  }

  sealed trait ArmyKind
  case object ImmuneSystem extends ArmyKind
  case object Infection extends ArmyKind

  case class Army(armyKind: ArmyKind, groups: Seq[GroupOfUnits])

  def armyKind[_ : P]: P[ArmyKind] = P( "Immune System".parseAs(ImmuneSystem) | "Infection".parseAs(Infection) )
  def army[_ : P]: P[Army] =
    P(
      armyKind
      ~ ":\n"
        ~ groupOfUnits.rep(min = 1, sep = "\n")
    ).map(Army.tupled)
  def armies[_ : P]: P[Seq[Army]] = P(army.rep(sep = "\n\n") ~ "\n" ~ End)

  def parseArmies(text: String): Seq[Army] = {
    parse(text, armies(_)).get.value
  }

  def parseGroup(line: String): GroupOfUnits = {
    parse(line, groupOfUnits(_)).get.value
  }

}

/**
  * --- Day 24: Immune System Simulator 20XX ---
  *
  * After a weird buzzing noise, you appear back at the man's cottage. He seems relieved to see his friend, but quickly notices that the little reindeer caught some kind of cold while out exploring.
  *
  * The portly man explains that this reindeer's immune system isn't similar to regular reindeer immune systems:
  *
  * The immune system and the infection each have an army made up of several groups; each group consists of one or more identical units. The armies repeatedly fight until only one army has units remaining.
  *
  * Units within a group all have the same hit points (amount of damage a unit can take before it is destroyed), attack damage (the amount of damage each unit deals), an attack type, an initiative (higher initiative units attack first and win ties), and sometimes weaknesses or immunities. Here is an example group:
  *
  * 18 units each with 729 hit points (weak to fire; immune to cold, slashing)
  * with an attack that does 8 radiation damage at initiative 10
  *
  * Each group also has an effective power: the number of units in that group multiplied by their attack damage. The above group has an effective power of 18 * 8 = 144. Groups never have zero or negative units; instead, the group is removed from combat.
  *
  * Each fight consists of two phases: target selection and attacking.
  *
  * During the target selection phase, each group attempts to choose one target. In decreasing order of effective power, groups choose their targets; in a tie, the group with the higher initiative chooses first. The attacking group chooses to target the group in the enemy army to which it would deal the most damage (after accounting for weaknesses and immunities, but not accounting for whether the defending group has enough units to actually receive all of that damage).
  *
  * If an attacking group is considering two defending groups to which it would deal equal damage, it chooses to target the defending group with the largest effective power; if there is still a tie, it chooses the defending group with the highest initiative. If it cannot deal any defending groups damage, it does not choose a target. Defending groups can only be chosen as a target by one attacking group.
  *
  * At the end of the target selection phase, each group has selected zero or one groups to attack, and each group is being attacked by zero or one groups.
  *
  * During the attacking phase, each group deals damage to the target it selected, if any. Groups attack in decreasing order of initiative, regardless of whether they are part of the infection or the immune system. (If a group contains no units, it cannot attack.)
  *
  * The damage an attacking group deals to a defending group depends on the attacking group's attack type and the defending group's immunities and weaknesses. By default, an attacking group would deal damage equal to its effective power to the defending group. However, if the defending group is immune to the attacking group's attack type, the defending group instead takes no damage; if the defending group is weak to the attacking group's attack type, the defending group instead takes double damage.
  *
  * The defending group only loses whole units from damage; damage is always dealt in such a way that it kills the most units possible, and any remaining damage to a unit that does not immediately kill it is ignored. For example, if a defending group contains 10 units with 10 hit points each and receives 75 damage, it loses exactly 7 units and is left with 3 units at full health.
  *
  * After the fight is over, if both armies still contain units, a new fight begins; combat only ends once one army has lost all of its units.
  *
  * For example, consider the following armies:
  *
  * Immune System:
  * 17 units each with 5390 hit points (weak to radiation, bludgeoning) with
  * an attack that does 4507 fire damage at initiative 2
  * 989 units each with 1274 hit points (immune to fire; weak to bludgeoning,
  * slashing) with an attack that does 25 slashing damage at initiative 3
  *
  * Infection:
  * 801 units each with 4706 hit points (weak to radiation) with an attack
  * that does 116 bludgeoning damage at initiative 1
  * 4485 units each with 2961 hit points (immune to radiation; weak to fire,
  * cold) with an attack that does 12 slashing damage at initiative 4
  *
  * If these armies were to enter combat, the following fights, including details during the target selection and attacking phases, would take place:
  *
  * Immune System:
  * Group 1 contains 17 units
  * Group 2 contains 989 units
  * Infection:
  * Group 1 contains 801 units
  * Group 2 contains 4485 units
  *
  * Infection group 1 would deal defending group 1 185832 damage
  * Infection group 1 would deal defending group 2 185832 damage
  * Infection group 2 would deal defending group 2 107640 damage
  * Immune System group 1 would deal defending group 1 76619 damage
  * Immune System group 1 would deal defending group 2 153238 damage
  * Immune System group 2 would deal defending group 1 24725 damage
  *
  * Infection group 2 attacks defending group 2, killing 84 units
  * Immune System group 2 attacks defending group 1, killing 4 units
  * Immune System group 1 attacks defending group 2, killing 51 units
  * Infection group 1 attacks defending group 1, killing 17 units
  *
  * Immune System:
  * Group 2 contains 905 units
  * Infection:
  * Group 1 contains 797 units
  * Group 2 contains 4434 units
  *
  * Infection group 1 would deal defending group 2 184904 damage
  * Immune System group 2 would deal defending group 1 22625 damage
  * Immune System group 2 would deal defending group 2 22625 damage
  *
  * Immune System group 2 attacks defending group 1, killing 4 units
  * Infection group 1 attacks defending group 2, killing 144 units
  *
  * Immune System:
  * Group 2 contains 761 units
  * Infection:
  * Group 1 contains 793 units
  * Group 2 contains 4434 units
  *
  * Infection group 1 would deal defending group 2 183976 damage
  * Immune System group 2 would deal defending group 1 19025 damage
  * Immune System group 2 would deal defending group 2 19025 damage
  *
  * Immune System group 2 attacks defending group 1, killing 4 units
  * Infection group 1 attacks defending group 2, killing 143 units
  *
  * Immune System:
  * Group 2 contains 618 units
  * Infection:
  * Group 1 contains 789 units
  * Group 2 contains 4434 units
  *
  * Infection group 1 would deal defending group 2 183048 damage
  * Immune System group 2 would deal defending group 1 15450 damage
  * Immune System group 2 would deal defending group 2 15450 damage
  *
  * Immune System group 2 attacks defending group 1, killing 3 units
  * Infection group 1 attacks defending group 2, killing 143 units
  *
  * Immune System:
  * Group 2 contains 475 units
  * Infection:
  * Group 1 contains 786 units
  * Group 2 contains 4434 units
  *
  * Infection group 1 would deal defending group 2 182352 damage
  * Immune System group 2 would deal defending group 1 11875 damage
  * Immune System group 2 would deal defending group 2 11875 damage
  *
  * Immune System group 2 attacks defending group 1, killing 2 units
  * Infection group 1 attacks defending group 2, killing 142 units
  *
  * Immune System:
  * Group 2 contains 333 units
  * Infection:
  * Group 1 contains 784 units
  * Group 2 contains 4434 units
  *
  * Infection group 1 would deal defending group 2 181888 damage
  * Immune System group 2 would deal defending group 1 8325 damage
  * Immune System group 2 would deal defending group 2 8325 damage
  *
  * Immune System group 2 attacks defending group 1, killing 1 unit
  * Infection group 1 attacks defending group 2, killing 142 units
  *
  * Immune System:
  * Group 2 contains 191 units
  * Infection:
  * Group 1 contains 783 units
  * Group 2 contains 4434 units
  *
  * Infection group 1 would deal defending group 2 181656 damage
  * Immune System group 2 would deal defending group 1 4775 damage
  * Immune System group 2 would deal defending group 2 4775 damage
  *
  * Immune System group 2 attacks defending group 1, killing 1 unit
  * Infection group 1 attacks defending group 2, killing 142 units
  *
  * Immune System:
  * Group 2 contains 49 units
  * Infection:
  * Group 1 contains 782 units
  * Group 2 contains 4434 units
  *
  * Infection group 1 would deal defending group 2 181424 damage
  * Immune System group 2 would deal defending group 1 1225 damage
  * Immune System group 2 would deal defending group 2 1225 damage
  *
  * Immune System group 2 attacks defending group 1, killing 0 units
  * Infection group 1 attacks defending group 2, killing 49 units
  *
  * Immune System:
  * No groups remain.
  * Infection:
  * Group 1 contains 782 units
  * Group 2 contains 4434 units
  *
  * In the example above, the winning army ends up with 782 + 4434 = 5216 units.
  *
  * You scan the reindeer's condition (your puzzle input); the white-bearded man looks nervous. As it stands now, how many units would the winning army have?
  *
  * Your puzzle answer was 26914.
  * --- Part Two ---
  *
  * Things aren't looking good for the reindeer. The man asks whether more milk and cookies would help you think.
  *
  * If only you could give the reindeer's immune system a boost, you might be able to change the outcome of the combat.
  *
  * A boost is an integer increase in immune system units' attack damage. For example, if you were to boost the above example's immune system's units by 1570, the armies would instead look like this:
  *
  * Immune System:
  * 17 units each with 5390 hit points (weak to radiation, bludgeoning) with
  * an attack that does 6077 fire damage at initiative 2
  * 989 units each with 1274 hit points (immune to fire; weak to bludgeoning,
  * slashing) with an attack that does 1595 slashing damage at initiative 3
  *
  * Infection:
  * 801 units each with 4706 hit points (weak to radiation) with an attack
  * that does 116 bludgeoning damage at initiative 1
  * 4485 units each with 2961 hit points (immune to radiation; weak to fire,
  * cold) with an attack that does 12 slashing damage at initiative 4
  *
  * With this boost, the combat proceeds differently:
  *
  * Immune System:
  * Group 2 contains 989 units
  * Group 1 contains 17 units
  * Infection:
  * Group 1 contains 801 units
  * Group 2 contains 4485 units
  *
  * Infection group 1 would deal defending group 2 185832 damage
  * Infection group 1 would deal defending group 1 185832 damage
  * Infection group 2 would deal defending group 1 53820 damage
  * Immune System group 2 would deal defending group 1 1577455 damage
  * Immune System group 2 would deal defending group 2 1577455 damage
  * Immune System group 1 would deal defending group 2 206618 damage
  *
  * Infection group 2 attacks defending group 1, killing 9 units
  * Immune System group 2 attacks defending group 1, killing 335 units
  * Immune System group 1 attacks defending group 2, killing 32 units
  * Infection group 1 attacks defending group 2, killing 84 units
  *
  * Immune System:
  * Group 2 contains 905 units
  * Group 1 contains 8 units
  * Infection:
  * Group 1 contains 466 units
  * Group 2 contains 4453 units
  *
  * Infection group 1 would deal defending group 2 108112 damage
  * Infection group 1 would deal defending group 1 108112 damage
  * Infection group 2 would deal defending group 1 53436 damage
  * Immune System group 2 would deal defending group 1 1443475 damage
  * Immune System group 2 would deal defending group 2 1443475 damage
  * Immune System group 1 would deal defending group 2 97232 damage
  *
  * Infection group 2 attacks defending group 1, killing 8 units
  * Immune System group 2 attacks defending group 1, killing 306 units
  * Infection group 1 attacks defending group 2, killing 29 units
  *
  * Immune System:
  * Group 2 contains 876 units
  * Infection:
  * Group 2 contains 4453 units
  * Group 1 contains 160 units
  *
  * Infection group 2 would deal defending group 2 106872 damage
  * Immune System group 2 would deal defending group 2 1397220 damage
  * Immune System group 2 would deal defending group 1 1397220 damage
  *
  * Infection group 2 attacks defending group 2, killing 83 units
  * Immune System group 2 attacks defending group 2, killing 427 units
  *
  * After a few fights...
  *
  * Immune System:
  * Group 2 contains 64 units
  * Infection:
  * Group 2 contains 214 units
  * Group 1 contains 19 units
  *
  * Infection group 2 would deal defending group 2 5136 damage
  * Immune System group 2 would deal defending group 2 102080 damage
  * Immune System group 2 would deal defending group 1 102080 damage
  *
  * Infection group 2 attacks defending group 2, killing 4 units
  * Immune System group 2 attacks defending group 2, killing 32 units
  *
  * Immune System:
  * Group 2 contains 60 units
  * Infection:
  * Group 1 contains 19 units
  * Group 2 contains 182 units
  *
  * Infection group 1 would deal defending group 2 4408 damage
  * Immune System group 2 would deal defending group 1 95700 damage
  * Immune System group 2 would deal defending group 2 95700 damage
  *
  * Immune System group 2 attacks defending group 1, killing 19 units
  *
  * Immune System:
  * Group 2 contains 60 units
  * Infection:
  * Group 2 contains 182 units
  *
  * Infection group 2 would deal defending group 2 4368 damage
  * Immune System group 2 would deal defending group 2 95700 damage
  *
  * Infection group 2 attacks defending group 2, killing 3 units
  * Immune System group 2 attacks defending group 2, killing 30 units
  *
  * After a few more fights...
  *
  * Immune System:
  * Group 2 contains 51 units
  * Infection:
  * Group 2 contains 40 units
  *
  * Infection group 2 would deal defending group 2 960 damage
  * Immune System group 2 would deal defending group 2 81345 damage
  *
  * Infection group 2 attacks defending group 2, killing 0 units
  * Immune System group 2 attacks defending group 2, killing 27 units
  *
  * Immune System:
  * Group 2 contains 51 units
  * Infection:
  * Group 2 contains 13 units
  *
  * Infection group 2 would deal defending group 2 312 damage
  * Immune System group 2 would deal defending group 2 81345 damage
  *
  * Infection group 2 attacks defending group 2, killing 0 units
  * Immune System group 2 attacks defending group 2, killing 13 units
  *
  * Immune System:
  * Group 2 contains 51 units
  * Infection:
  * No groups remain.
  *
  * This boost would allow the immune system's armies to win! It would be left with 51 units.
  *
  * You don't even know how you could boost the reindeer's immune system or what effect it might have, so you need to be cautious and find the smallest boost that would allow the immune system to win.
  *
  * How many units does the immune system have left after getting the smallest boost it needs to win?
  *
  * Your puzzle answer was 862.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day24 extends Utils {

  import Day24Parser._

  lazy val inputTextFromResource : String =
    readResourceAsString("day24.txt")

  lazy val initialArmies: Seq[Army] = parseArmies(inputTextFromResource)

  lazy val initialAutonomousGroups: List[AutonomousGroup] =
    initialArmies
      .flatMap{ case Army(armyKind, groups) => groups.map{ g => AutonomousGroup(g.id, armyKind, g)}}
      .toList

  type Damage = Int

  /**
    * The attacking group chooses to target the group in the enemy army to which it would deal the most damage
    * (after accounting for weaknesses and immunities, but not accounting for whether the defending group has
    * enough units to actually receive all of that damage).
    */
  def estimateDamageForTargetSelection(attacker: GroupOfUnits, target: GroupOfUnits): Damage = {
    if(target.immuneTo.contains(attacker.attackKind))
      0
    else if(target.weakTo.contains(attacker.attackKind))
      attacker.effectivePower * 2
    else
      attacker.effectivePower
  }

  /** Selects 0..1 group for attack.*/
  def attemptSelectTarget(attacker: AutonomousGroup, otherGroups: List[AutonomousGroup]): (Option[AutonomousGroup], List[AutonomousGroup]) = {
    val withDamage = otherGroups.map(g =>
      (g, (estimateDamageForTargetSelection(attacker.groupOfUnits, g.groupOfUnits), g.groupOfUnits.effectivePower, g.groupOfUnits.initiative))
    )
    val sorted = withDamage.sortBy(_._2)(reverseOrder).toList
    sorted match {
      case Nil =>
        (None, Nil)
      case (_, a1):: (_, a2) :: _ if a1 == a2 =>
        (None, otherGroups)
      case (_, (0, _ ,_)):: _ => // there is no damage we can apply. Removing ourselves from fight
        (None, otherGroups)
      case (g1, _):: t =>
        (Some(g1), t.map(_._1))
    }
  }

  def enemy(armyKind: ArmyKind): ArmyKind = armyKind match {
    case ImmuneSystem => Infection
    case Infection => ImmuneSystem
  }

  def targetSelection(groupsSorted: List[AutonomousGroup],
    armies: Map[ArmyKind, List[AutonomousGroup]],
    selection: List[(AutonomousGroup, Option[AutonomousGroup])]  = Nil
  ): List[(AutonomousGroup, Option[AutonomousGroup])] = groupsSorted match {
    case Nil =>
      selection
    case attacker :: tail =>
      val enemyKind = enemy(attacker.armyKind)
      val (selected, rest) = attemptSelectTarget(attacker, armies(enemyKind))
      targetSelection(tail, armies.updated(enemyKind, rest), attacker -> selected :: selection)
  }

  type State = List[AutonomousGroup]

  def attack(list: List[(AutonomousGroup, Option[AutonomousGroup])], map: Map[Id, AutonomousGroup]): State = list match {
    case Nil => map.values.toList
    case (_, None) :: tail =>
      attack(tail, map)
    case (AutonomousGroup(attackerId, _, _), Some(AutonomousGroup(targetId, _, _))) :: tail =>
      val attacker = map(attackerId)
      val target = map(targetId)
      if(attacker.groupOfUnits.unitsCount > 0) {
        val damage = estimateDamageForTargetSelection(attacker.groupOfUnits, target.groupOfUnits)
        val targetState = target.applyDamage(damage)
        attack(tail, map.updated(target.id, targetState))
      } else
        attack(tail, map)
  }

  def fight(state: State): State = {
    val sorted = state.sortBy(g => (-g.groupOfUnits.effectivePower, -g.groupOfUnits.initiative))
    val map = state.map(ag => (ag.id, ag)).toMap
    val selections = targetSelection(sorted, sorted.groupBy(_.armyKind), Nil)
    val afterAttack = attack(selections.sortBy(-_._1.groupOfUnits.initiative), map)
    eliminateEmptyGroups(afterAttack)
  }

  def eliminateEmptyGroups(state: State): State =
    state.filterNot(_.groupOfUnits.unitsCount == 0)

  def countArmies(state: State): Int =
    state.map(_.armyKind).distinct.size

  def countUnits(state: State): Int =
    state.map(_.groupOfUnits.unitsCount).sum

  def countUnitsOfImmuneSystem(state: State): Int =
    state.filter(_.armyKind == ImmuneSystem).map(_.groupOfUnits.unitsCount).sum

  def showState(state: State): String = {
    "----------\n" +
    state.groupBy(_.armyKind)
      .map{
        case (armyKind: ArmyKind, g) =>
          "" + armyKind + ":\n" + g.sortBy(_.id).map(ag =>
            "" + ag.id + ": " + ag.groupOfUnits.unitsCount + " (" + ag.groupOfUnits.effectivePower + ")"
          ).mkString("\n")
      }
      .mkString("\n")
  }

  // 59060 - too high
  // 26683 - too low
  // 26914
  lazy val answer1: Long = {
    val lastState = unfoldUntil(fight)(s => countArmies(s) == 1)(initialAutonomousGroups)
    println(showState(lastState))
    countUnits(lastState)
  }

  // Part 2

  def game(boost: Int): Int = {
    println("Trying boost " + boost)
    val boosted = initialAutonomousGroups.map{ag =>
      if(ag.armyKind == ImmuneSystem)
        {
          ag.copy(groupOfUnits = ag.groupOfUnits.copy(attackDamage = ag.groupOfUnits.attackDamage + boost))
        }
      else
        ag
    }
    val lastState = unfoldUntil(fight)(s => countArmies(s) == 1)(boosted)
    countUnitsOfImmuneSystem(lastState)
  }

  // 862 (the game stucks at boosts of 23-47)
  lazy val answer2: Long = {
    val isImmuneSystemWin: Int => Boolean = boost => Try(game(boost) > 0).recover{ case _ => false}.get
    val guessAbove = 50
    require(isImmuneSystemWin(guessAbove))
    val boost = findMinArgForPredicate(isImmuneSystemWin)(0, guessAbove)
    game(boost)
  }

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
