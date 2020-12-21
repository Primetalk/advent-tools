package org.primetalk.advent2020

import org.primetalk.advent.tools.Utils

import scala.annotation.tailrec

/**
  * https://adventofcode.com/2020/day/21
  * --- Day 21: Allergen Assessment ---
  *
  * You reach the train's last stop and the closest you can get to your vacation island without getting wet. There aren't even any boats here, but nothing can stop you now: you build a raft. You just need a few days' worth of food for your journey.
  *
  * You don't speak the local language, so you can't read any ingredients lists. However, sometimes, allergens are listed in a language you do understand. You should be able to use this information to determine which ingredient contains which allergen and work out which foods are safe to take with you on your trip.
  *
  * You start by compiling a list of foods (your puzzle input), one food per line. Each line includes that food's ingredients list followed by some or all of the allergens the food contains.
  *
  * Each allergen is found in exactly one ingredient. Each ingredient contains zero or one allergen. Allergens aren't always marked; when they're listed (as in (contains nuts, shellfish) after an ingredients list), the ingredient that contains each listed allergen will be somewhere in the corresponding ingredients list. However, even if an allergen isn't listed, the ingredient that contains that allergen could still be present: maybe they forgot to label it, or maybe it was labeled in a language you don't know.
  *
  * For example, consider the following list of foods:
  *
  * mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
  * trh fvjkl sbzzf mxmxvkd (contains dairy)
  * sqjhc fvjkl (contains soy)
  * sqjhc mxmxvkd sbzzf (contains fish)
  *
  * The first food in the list has four ingredients (written in a language you don't understand): mxmxvkd, kfcds, sqjhc, and nhms. While the food might contain other allergens, a few allergens the food definitely contains are listed afterward: dairy and fish.
  *
  * The first step is to determine which ingredients can't possibly contain any of the allergens in any food in your list. In the above example, none of the ingredients kfcds, nhms, sbzzf, or trh can contain an allergen. Counting the number of times any of these ingredients appear in any ingredients list produces 5: they all appear once each except sbzzf, which appears twice.
  *
  * Determine which ingredients cannot possibly contain any of the allergens in your list. How many times do any of those ingredients appear?
  *
  * Your puzzle answer was 2317.
  * --- Part Two ---
  *
  * Now that you've isolated the inert ingredients, you should have enough information to figure out which ingredient contains which allergen.
  *
  * In the above example:
  *
  *     mxmxvkd contains dairy.
  *     sqjhc contains fish.
  *     fvjkl contains soy.
  *
  * Arrange the ingredients alphabetically by their allergen and separate them by commas to produce your canonical dangerous ingredient list. (There should not be any spaces in your canonical dangerous ingredient list.) In the above example, this would be mxmxvkd,sqjhc,fvjkl.
  *
  * Time to stock your raft with supplies. What is your canonical dangerous ingredient list?
  *
  * Your puzzle answer was kbdgs,sqvv,slkfgq,vgnj,brdd,tpd,csfmb,lrnz.
  *
  * Both parts of this puzzle are complete! They provide two gold stars: **
  */
object Day2021 extends Utils{

  val input: IndexedSeq[String] = readResourceLines("day21.txt")

  type Allergen = String
  type Food = String

  case class FoodRecord(food: Set[Food], allergens: Set[Allergen])

  def parseLine(line: String): FoodRecord = {
    val containsIndex = line.indexOf(" (contains ")
    val food = line.substring(0, containsIndex).split(' ').toSet
    val allergens1 = line.substring(containsIndex + 11)
    val allergens = allergens1
      .substring(0, allergens1.length - 1)
      .split(", ").toSet
    FoodRecord(food, allergens)
  }

  val records: IndexedSeq[FoodRecord] = input.map(parseLine)

  def backwardIndex(records: IndexedSeq[FoodRecord]): Map[Allergen, Set[Food]] = {
    val allAllergens: Set[Allergen] = records.map(_.allergens).reduce(_ ++ _)

    allAllergens
      .toList
      .map{
        allergen =>
          (allergen,
            records
              .filter(_.allergens.contains(allergen))
              .map(_.food)
              .reduce(_ intersect _)
          )
      }
      .toMap
  }

  @tailrec
  def eliminateUniqueAllergenFood(m: Map[Allergen, Set[Food]], res: List[(Allergen, Food)]): Map[Allergen, Food] = {
    if(m.isEmpty)
      res.toMap
    else {
      val (mWithUniqueFood, mWithNotUniqueFood) = m.partition(_._2.size == 1)
      if(mWithUniqueFood.isEmpty) {
        throw new IllegalArgumentException(s"Couldn't make unique \n${m.mkString("\n")}")
      } else {
        val allergenToFoodList =
          mWithUniqueFood
            .keys
            .map(allergen =>
              (allergen, m(allergen).head)
            )
            .toList
        val identifiedFoodSet = allergenToFoodList.map(_._2).toSet
        eliminateUniqueAllergenFood(
          mWithNotUniqueFood
            .view.mapValues(_ -- identifiedFoodSet)
            .toMap,
          allergenToFoodList reverse_::: res
        )
      }
    }
  }

  val mapAllergenToFood: Map[Allergen, Set[Food]] = backwardIndex(records)

  val uniqueIngredients: Map[Allergen, Food] = eliminateUniqueAllergenFood(mapAllergenToFood, Nil)

  val allergyFood: Set[Food] = uniqueIngredients.values.toSet

  lazy val answer1: Int =
    records
      .map(_.food -- allergyFood)
      .map(_.size)
      .sum

  //Part 2
  lazy val answer2: String =
    uniqueIngredients
      .toList
      .sortBy(_._1)
      .map(_._2)
      .mkString(",")

  def main(args: Array[String]): Unit = {
    println("Answer1: " + answer1)
    println("Answer2: " + answer2)
  }

}
