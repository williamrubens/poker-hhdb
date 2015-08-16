package adapters

import org.scalatest._
import org.scalatest.Assertions._

/**
 * Created by willrubens on 01/08/15.
 */
class PotManagerTest extends FlatSpec with Matchers  {

  "A PotManager" should "report the correct pot odds" in {
    val potManager = new PotManager

    potManager.setPlayerPot(0, 1)
    potManager.setPlayerPot(1, 2)

    assertResult(0.25)(potManager.potOdds(0))

  }

  def build5PlayerPotManager =  {
    val potManager = new PotManager

    potManager.setPlayerPot(0, 1)
    potManager.setPlayerPot(1, 2)
    potManager.setPlayerPot(2, 3)
    potManager.setPlayerPot(3, 4)
    potManager.setPlayerPot(4, 0)

    potManager
  }

  "A PotManager" should "report the correct pot odds for player 0" in {
    
    assertResult(3.0 / 13.0)(build5PlayerPotManager.potOdds(0))

  }

  "A PotManager" should "report the correct pot odds for player 1" in {

    assertResult(2.0 / 12.0)(build5PlayerPotManager.potOdds(1))

  }

  "A PotManager" should "report the correct pot odds for player 2" in {

    assertResult(1.0 / 11.0)(build5PlayerPotManager.potOdds(2))

  }

  "A PotManager" should "report the correct pot odds for player 3" in {

    assertResult(0.0 / 10.0)(build5PlayerPotManager.potOdds(3))

  }

  "A PotManager" should "report the correct pot odds for player 4" in {

    assertResult(4.0 / 14.0)(build5PlayerPotManager.potOdds(4))

  }

}
