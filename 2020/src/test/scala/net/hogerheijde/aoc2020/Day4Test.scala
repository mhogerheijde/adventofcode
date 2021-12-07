package net.hogerheijde.aoc2020

import net.hogerheijde.aoc.util.Parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day4Test  extends AnyWordSpec with Matchers {

  "Day 4" should {

    "parse correctly" in {
      val input =
        """a:a1 b:b1
          |c:c1
          |
          |a:a2
          |b:b2 c:c2
          |
          |""".stripMargin

      Parser.parse(Day4.property(_))("ecl:gry ") should be (Some("ecl", "gry"))
      Parser.parse(Day4.property(_))("ecl:gry\nfoo:bar") should be (Some("ecl", "gry"))
      Parser.parse(Day4.credentials(_))("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm") should
        be (Some(Map(
          "ecl" -> "gry",
          "pid" -> "860033327",
          "eyr" -> "2020",
          "hcl" -> "#fffffd",
          "byr" -> "1937",
          "iyr" -> "2017",
          "cid" -> "147",
          "hgt" -> "183cm",
        )))
      Parser.parse(Day4.credentials(_))("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n") should
        be (Some(Map(
          "ecl" -> "gry",
          "pid" -> "860033327",
          "eyr" -> "2020",
          "hcl" -> "#fffffd",
          "byr" -> "1937",
          "iyr" -> "2017",
          "cid" -> "147",
          "hgt" -> "183cm",
        )))

      Day4.parse(input) should be (
        Seq(
          Map("a" -> "a1", "b" -> "b1", "c" -> "c1"),
          Map("a" -> "a2", "b" -> "b2", "c" -> "c2"),
        )
      )
    }
    "part 1 count correctly" in  {
      val input =
          """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
            |byr:1937 iyr:2017 cid:147 hgt:183cm
            |
            |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
            |hcl:#cfa07d byr:1929
            |
            |hcl:#ae17e1 iyr:2013
            |eyr:2024
            |ecl:brn pid:760753108 byr:1931
            |hgt:179cm
            |
            |hcl:#cfa07d eyr:2025 pid:166559648
            |iyr:2011 ecl:brn hgt:59in
            |
            |""".stripMargin

      val credentials = Day4.parse(input)
      Day4.part1(credentials) should be (2)
    }


    "part 2 counts valid correctly" in {
        val input =
          """pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
            |hcl:#623a2f
            |
            |eyr:2029 ecl:blu cid:129 byr:1989
            |iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
            |
            |hcl:#888785
            |hgt:164cm byr:2001 iyr:2015 cid:88
            |pid:545766238 ecl:hzl
            |eyr:2022
            |
            |iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
            |
            |""".stripMargin
      val credentials = Day4.parse(input)
      Day4.part2(credentials) should be (4)
    }

    "part 2 counts invalid correctly" in {
        val input =
          """eyr:1972 cid:100
            |hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
            |
            |iyr:2019
            |hcl:#602927 eyr:1967 hgt:170cm
            |ecl:grn pid:012533040 byr:1946
            |
            |hcl:dab227 iyr:2012
            |ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
            |
            |hgt:59cm ecl:zzz
            |eyr:2038 hcl:74454a iyr:2023
            |pid:3556412378 byr:2007
            |
            |""".stripMargin
      val credentials = Day4.parse(input)
      Day4.part2(credentials) should be (0)
    }

    "byr checks" in {
//      byr (Birth Year) - four digits; at least 1920 and at most 2002.
      Range(0, 1919).inclusive.foreach { year =>
        ValidityChecker.byr(Map("byr" -> year.toString)) should be (false)
      }
      Range(1920, 2002).inclusive.foreach { year =>
        ValidityChecker.byr(Map("byr" -> year.toString)) should be (true)
      }
      Range(2003, 10000).inclusive.foreach { year =>
        ValidityChecker.byr(Map("byr" -> year.toString)) should be (false)
      }
    }

    "iyr checks" in {
//      iyr (Issue Year) - four digits; at least 2010 and at most 2020.
      Range(0, 2009).inclusive.foreach { year =>
        ValidityChecker.iyr(Map("iyr" -> year.toString)) should be (false)
      }
      Range(2010, 2020).inclusive.foreach { year =>
        ValidityChecker.iyr(Map("iyr" -> year.toString)) should be (true)
      }
      Range(2021, 10000).inclusive.foreach { year =>
        ValidityChecker.iyr(Map("iyr" -> year.toString)) should be (false)
      }
    }

    "eyr checks" in {
//      eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
      Range(0, 2019).inclusive.foreach { year =>
        ValidityChecker.eyr(Map("eyr" -> year.toString)) should be (false)
      }
      Range(2020, 2030).inclusive.foreach { year =>
        ValidityChecker.eyr(Map("eyr" -> year.toString)) should be (true)
      }
      Range(2031, 10000).inclusive.foreach { year =>
        ValidityChecker.eyr(Map("eyr" -> year.toString)) should be (false)
      }
    }

    "hgt checks" in {
//      hgt (Height) - a number followed by either cm or in:
//      If cm, the number must be at least 150 and at most 193.
//      If in, the number must be at least 59 and at most 76.

      Range(0, 149).inclusive.foreach { height =>
        ValidityChecker.hgt(Map("hgt" -> s"${height}cm")) should be (false)
      }
      Range(150, 193).inclusive.foreach { height =>
        println(s"${height}cm")
        ValidityChecker.hgt(Map("hgt" -> s"${height}cm")) should be (true)
      }
      Range(194, 1000).inclusive.foreach { height =>
        ValidityChecker.hgt(Map("hgt" -> s"${height}cm")) should be (false)
      }

      Range(0, 58).inclusive.foreach { height =>
        ValidityChecker.hgt(Map("hgt" -> s"${height}in")) should be (false)
      }
      Range(59, 76).inclusive.foreach { height =>
        ValidityChecker.hgt(Map("hgt" -> s"${height}in")) should be (true)
      }
      Range(77, 500).inclusive.foreach { height =>
        ValidityChecker.hgt(Map("hgt" -> s"${height}in")) should be (false)
      }
    }

  }


}
