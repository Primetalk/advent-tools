package org.primetalk.advent2020

class Day2004Test extends BaseTest {

  import Day2004._

  behavior of "Day2004Test"

  it should "answer1" in {
    Day2004.answer1 shouldBe 204
  }

  it should "answer2" in {
//    Day2004.answer2 shouldBe 0
  }

  "individual rules" should "work as expected" in {
    byr("2002") shouldBe true
    byr("2003") shouldBe false

    hgt("60in") shouldBe true
    hgt("190cm")  shouldBe true
    hgt("190in")   shouldBe false
    hgt("190")  shouldBe false

    hcl("#123abc") shouldBe true
    hcl("#123abz") shouldBe false
    hcl("123abc") shouldBe false

    ecl("brn") shouldBe true
    ecl("wat") shouldBe false

    pid("000000001") shouldBe true
    pid("0123456789") shouldBe false
  }

  "invalid passports" should "be invalid" in {
    isValid2(passportParse("eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926")) shouldBe false

    isValid2(passportParse("iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946")) shouldBe false
    isValid2(passportParse("hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277")) shouldBe false
    isValid2(passportParse("hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007")) shouldBe false
  }
  "valid passports" should "be valid" in {
    isValid2(passportParse("pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f")) shouldBe true
    isValid2(passportParse("eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm")) shouldBe true
    isValid2(passportParse("pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f")) shouldBe true
    isValid2(passportParse("hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022")) shouldBe true
    isValid2(passportParse("iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")) shouldBe true
  }
}
