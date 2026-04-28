package ex1

import ex1.Parsers.charParser
import org.scalatest.matchers.should.Matchers.*

class ParserScalaTest extends org . scalatest . funsuite . AnyFunSuite :
  def parser = new BasicParser(Set('a', 'b', 'c'))
  def parserNE = new NonEmptyParser(Set('0', '1'))
  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
  def sparser: Parser[Char] = "abc".charParser

  test("Basic Parser"):
    parser.parseAll("aabc".toList) shouldBe true
    parser.parseAll("aabcdc".toList) shouldBe false
    parser.parseAll("".toList) shouldBe true

  test("notEmpty Parser"):
    parserNE.parseAll("0101".toList) shouldBe true
    parserNE.parseAll("0123".toList) shouldBe false
    parserNE.parseAll(List()) shouldBe false

  test("NotTwoConsecutive Parser"):
    parserNTC.parseAll("XYZ".toList) shouldBe true
    parserNTC.parseAll("XYYZ".toList) shouldBe false
    parserNTC.parseAll("".toList) shouldBe true

  test("NotEmptyAndNotTwoConsecutive Parser"):
    parserNTCNE.parseAll("XYZ".toList) shouldBe true
    parserNTCNE.parseAll("XYYZ".toList) shouldBe false
    parserNTCNE.parseAll("".toList) shouldBe false

  test("String Parser"):
    sparser.parseAll("aabc".toList) shouldBe true
    sparser.parseAll("aabcdc".toList) shouldBe false
    sparser.parseAll("".toList) shouldBe true