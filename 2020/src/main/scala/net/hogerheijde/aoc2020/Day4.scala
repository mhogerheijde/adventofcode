package net.hogerheijde.aoc2020

import scala.util.Try

import fastparse._
import fastparse.NoWhitespace._
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2020.Day4.Credentials
import net.hogerheijde.aoc.util.Implicits.IntHelpers

object ValidityChecker {
  /*
byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.
 */



  val byr: Credentials => Boolean = c => c.get("byr")
    .exists { value =>
      Try {
        val i = Integer.parseInt(value)
        i >= 1920 && i <= 2002
      }.getOrElse(false)
    }
  val iyr: Credentials => Boolean = c => c.get("iyr")
    .exists { value =>
      Try {
        val i = Integer.parseInt(value)
        i >= 2010 && i <= 2020
      }.getOrElse(false)
    }
  val eyr: Credentials => Boolean = c => c.get("eyr")
    .exists { value =>
      Try {
        val i = Integer.parseInt(value)
        i >= 2020 && i <= 2030
      }.getOrElse(false)
    }

  val inches = "([0-9]+)in".r
  val cms = "([0-9]+)cm".r
  val hgt: Credentials => Boolean = c => c.get("hgt")
    .exists {
      case cms(height) => Integer.parseInt(height).betweenInclusive(150, 193)
      case inches(height) => Integer.parseInt(height).betweenInclusive(59, 76)
      case _ => false
    }

  val hclfmt = "#[0-9a-f]{6}".r
  val hcl: Credentials => Boolean =  c => c.get("hcl").exists(hclfmt.matches)

  val colors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  val ecl: Credentials => Boolean =  c => c.get("ecl").exists(colors.contains)

  val pidfmt = "[0-9]{9}".r
  val pid: Credentials => Boolean =  c => c.get("pid").exists(pidfmt.matches)

  private val checks = Set(
    byr,
    iyr,
    eyr,
    hgt,
    hcl,
    ecl,
    pid,
  )
  def apply(credentials: Credentials): Boolean = checks.forall(_(credentials))
}

object Day4 extends Day[Int, Int] {

  type Credentials = Map[String, String]
  type Model = Seq[Credentials]

  override def parse(input: String): Day4.Model = {
    Parser.parse(credentialss(_))(input).get
  }

  val fields = Set(
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid",
    "cid",
  )

  val requiredFields = fields - "cid"

  override def part1(input: Day4.Model): Int = input.count(isValidPt1(_))

  override def part2(input: Day4.Model): Int = input.count(isValidPt2(_))

  def isValidPt1(credentials: Credentials): Boolean = (requiredFields -- credentials.keySet) == Set.empty

  def isValidPt2(credentials: Credentials): Boolean = {
//    isValidPt1(credentials) &&
      ValidityChecker(credentials)
  }

  // -- show

  def formatCredentials(credentials: Credentials): String = {
    s"""====
      |byr = ${credentials.get("byr")}
      |iyr = ${credentials.get("iyr")}
      |eyr = ${credentials.get("eyr")}
      |hgt = ${credentials.get("hgt")}
      |hcl = ${credentials.get("hcl")}
      |ecl = ${credentials.get("ecl")}
      |pid = ${credentials.get("pid")}
      |cid = ${credentials.get("cid")}
      |""".stripMargin
  }

  // -- parsers
  def key[_: P]: P[String] = P(CharsWhile(c => c != ':' && c != '\n').rep.!)
  def value[_: P]: P[String] = P(CharsWhile(c => c !=' ' && c != '\n').rep.!)
  def property[_: P]: P[(String, String)] = P(key ~ ":" ~ value ~ (" " | "\n").?)
  def credentials[_: P]: P[Credentials] = P(property.rep).map { _.toMap }
  def credentialss[_: P]: P[Seq[Credentials]] = P( Start ~ (credentials ~ "\n").rep ~ End) //.map(Seq(_))
}
