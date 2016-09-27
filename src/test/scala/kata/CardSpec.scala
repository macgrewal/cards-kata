package kata

import org.scalatest.{Matchers, WordSpec}

class CardSpec extends WordSpec with Matchers {

  "Calling .getHighest" when {

    "an empty sequence is passed in" should {
      "return an empty list" in {
        Cards.getHighest(Nil) shouldBe None
      }
    }

    "only 1 card is passed in" should {
      "return a the value of the card" in {
        Cards.getHighest(Seq(1)) shouldBe Some(1)
      }
    }

    "multiple cards are passed in" should {
      "return a the highest value card from list" in {
        val cards = Seq(1, 2)
        Cards.getHighest(cards) shouldBe Some(2)
      }

      "return a the highest value card from list when all cards are the same value" in {
        val cards = Seq(2, 2, 2, 2, 2, 2, 2)
        Cards.getHighest(cards) shouldBe Some(2)
      }

      "return a the highest value card regardless of the position" in {
        val cards = Seq(2, 2, 2, 2, 8, 2, 2)
        Cards.getHighest(cards) shouldBe Some(8)
      }
    }
  }

  "Calling .getPairs" when {

    "No cards are present" should {
      "return an empty list" in {
        Cards.getPairs(Nil) shouldBe Nil
      }
    }

    "No pairs are present" should {
      "return an empty list" in {
        Cards.getPairs(Seq(1, 2, 3, 4, 5)) shouldBe Nil
      }
    }

    "1 pair is present" should {
      "return a list with the value of the pair" in {
        Cards.getPairs(Seq(1, 2, 3, 4, 5, 1)) shouldBe Seq(1)
      }
    }

    "2 different pairs are present" should {
      "return a list with the value of the pair" in {
        Cards.getPairs(Seq(1, 2, 3, 4, 4, 1)) shouldBe Seq(1, 4)
      }
    }

    "2 of the same pairs are present" should {
      "return a list with the same value twice" in {
        Cards.getPairs(Seq(1, 2, 3, 1, 1, 1)) shouldBe Seq(1, 1)
      }
    }

    "unique and repeating pairs are present" should {
      "return a list with all the pairs" in {
        Cards.getPairs(Seq(1,2,1,2,1,1,5)) shouldBe Seq(1, 1, 2)
      }
    }
  }

  "Calling .getHighestPairs" when {

    "No cards are present" should {
      "returns None" in {
        Cards.getHighestPairs(Nil) shouldBe None
      }
    }

    "No pairs are present" should {
      "return None" in {
        Cards.getHighestPairs(Seq(1, 2, 3, 4, 5)) shouldBe None
      }
    }

    "1 pair is present" should {
      "returns the value of the pair" in {
        Cards.getHighestPairs(Seq(1, 2, 3, 4, 5, 1)) shouldBe Some(1)
      }
    }

    "2 different pairs are present" should {
      "return the value of the higher valued pair" in {
        Cards.getHighestPairs(Seq(1, 2, 3, 4, 4, 1)) shouldBe Some(4)
      }
    }

    "2 of the same pairs are present" should {
      "return the value of the pair" in {
        Cards.getHighestPairs(Seq(1, 2, 3, 1, 1, 1)) shouldBe Some(1)
      }
    }

    "unique and repeating pairs are present" should {
      "return the value of the higher valued pair" in {
        Cards.getHighestPairs(Seq(1,2,1,2,1,1,5)) shouldBe Some(2)
      }
    }
  }

}
