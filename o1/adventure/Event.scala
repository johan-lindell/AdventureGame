package o1.adventure
import scala.util.Random



/*Events can be added to areas, events are generally initiated by the player having a ceratin item in their inventory*/
abstract class Event(val game: Adventure, val conversationList: Vector[String]) {
  var eventDone = false
  var conversationDone = false
  def guess(guessed: String): String = "Unkown command"

  def canBeStarted = !this.eventDone || !game.isOver

  var conversationCounter = -1

  def turnCheck(): String

  //interact initiates event or returns nothing interesting if event is done
  def interact: String = {
    if (!this.eventDone) {
      this.game.activeEvent = Some(this)
      this.nextLine
    } else "nothing interesting to interact with"
  }

  //handles the conversation part of event
  def nextLine: String = {
    this.conversationCounter += 1
    if (!this.eventDone && !(this.conversationCounter >= this.conversationList.length - 1)) {
      conversationList(conversationCounter) + "\n(n)"
    } else if (!this.eventDone) {
      conversationList(this.conversationList.length - 1)
    } else {
      conversationDone = true
      ""
    }
  }

  //if event is not a fight event gives unknown command, figh events override this function
  def fight(action: String) = "unknown command: " + action
}


//Ossinlampi event is a special type of puzzle event, readme.txt gives more info on this, targetArea is the are that the puzzle items need to be brought to
class Ossinlampi(val player: Player, game: Adventure, val targetArea: Area) extends Event(game, Vector("You see a person standing by the lake... \nIt's Esa Saarinen, what could he possibly do here?\n(Type n for next dialogue)","Esa: Good friend! Welcome!\nIt is really nice that you are here, at Ossinlampi\nI have a specific issue I need your help with...",
    "Esa: I have some objects i need you to carry to my lecture hall.\nThese are:\n(1) A Mechanical engineer\n(2) A chicken\n(3) My jacket",
    "Esa: Because these objects are difficult to carry you can only have one in your inventory at a time.\nYou cannot leave the chicken with the mechanical engineer or he will eat it!\nYou also cannot leave the chicken with my jacket as it will duce all over it.",
    "Esa: Your task is to bring each object safetly to the Kandilafka\nIf you do this I will reward you handsomly", "Esa: Go on, we dont have all day, remember you can only pick up one item at a time.\nAnd you cannot leave the mechanical engineer with the chicken or\nthe chicken with the jacket alone!")) {


  val items = Vector(new Item("chicken", "It's a regular chicken"), new Item("mechanical engineer", "Koneeeeeee!"), new Item("jacket", "Esa's jacket"))


  private def isFail(area: Area): Boolean = ((area.contains("chicken") && area.contains("mechanical engineer")) || (area.contains("jacket") && area.contains("chicken"))) && player.location != area


  //if a wrong move is made resets the puzzle
  private def areaFailed(area: Area): String = {
      val itemsInTarget = this.items.filter(item => area.contains(item.name))
      this.items.foreach(item => area.removeItem(item.name))
      this.items.foreach(item => this.player.destoryItem(item.name))
      items.foreach(item => this.game.ossinlampi.addItem(item))
      s"Oh no! You left the ${itemsInTarget(0)} alone with the ${itemsInTarget(1)}"
  }
  //checks if moves are valid calls areaFailed if not, also checks when event is won
  def turnCheck(): String = {
    if (this.isFail(targetArea)) {
      areaFailed(targetArea)
    } else if (this.isFail(this.game.ossinlampi)) {
      areaFailed(this.game.ossinlampi)
    } else
    if (this.items.forall(item => targetArea.contains(item.name))) {
      this.game.activeEvent = None
      this.eventDone = true
      this.player.addItem(new Item("mysterious key", "There is some poorly written text on the key, you barely make out: \nS##K#I"))
      this.items.foreach(item => this.targetArea.removeItem(item.name))
      "Congratulations you have succesfully transported the items for Esa!\nEsa hands you a mysterious key"
    } else {
      " "
    }
  }
}

/*this event has 3 steps
* 1: Event gets initiated
* 2: Player figures out how to get back in time which in turn gives player keys that initiate the fight event
* 3: when the fight event is completed gives players home keys*/
class Smokki(val player: Player, game: Adventure) extends Event(game, Vector("You unlock the door to smökki.\nYou see someone cleaning up after yesterdays shenanigans.\nYou ask if he has seen any keys laying around.",
  s"Cleaning teekkari: Yes we found some keys laying around, im sorry to say that they have been thrown away.\n If you would have been here before 16:00 I could have given them to you...", "Damn, if only I could find a way to go back in time I could get my keys...")) {

  override def canBeStarted = (this.player.has("mysterious key") && !this.eventDone && this.game.turnCount > 4) || this.player.has("spaghetti")

  //checks what part of the event should be initiated
  override def interact: String = {
    if (this.player.has("spaghetti")) {
      this.player.addItem(new Item("home keys", "Keys to my home in jmt1"))
      this.game.activeEvent = None
      this.eventDone = true
      "You hand over the spaghetti.\nPerson: mmmmmmm, thank you! Here are your keys as promised."
    } else if (!this.eventDone) {
      this.player.destoryItem("mysterious key")
      this.game.activeEvent = Some(this)
      this.nextLine
    } else super.interact
  }

  //gives player the figh event initiating item if current turn is low enough
  def turnCheck(): String =  {
    if (this.game.turnCount <= 4) {
      this.game.activeEvent = None
      this.player.addItem(new Item("cash", "exactly 3 euros"))
      "You see a person holding your keys.\nYou: Hey, those are my keys!\nPerson: If you fetch me some spaghetti I will give them..\n\nThe person hands you some cash, you dont argue and decide to get him some spaghetti."
    } else ""
  }


  //returns event initiating dialogue
  override def nextLine: String = {
    if (this.game.turnCount > 4) super.nextLine
    else ""
  }
}


//last event, simple riddle event
class HomeEvent(val answer: String, val player: Player, game: Adventure) extends Event(game, Vector("You make your way to your appartement.\nYou boot up your computer…You stare at the enter password page, and realize you can't remember your password!",
  "Luckily, you have written down a clue and remember that the password is 2 digits long.\nThe clue reads:\nHow can the number four be half of five?\n\n(Enter the password using the command: guess <password here>)")) {

  override def canBeStarted: Boolean = this.player.has("home keys") && !this.eventDone

  override def turnCheck(): String = ""


  override def guess(guessed: String): String = {
    if (guessed == this.answer) {
      this.game.isWon = true
      ""
    } else {
      "incorrect password, try again"+
      "\n\nClue: How can the number four be half of five?"
    }
  }
}


//fight event starts with conversation and ends with fight
class FightEvent(val player: Player, game: Adventure)
  extends Event(game, Vector("Oh no you are too late all the spaghetti is gone.\n There is a person who has not yet started eating and ask him if you could buy hes spaghetti",
    "Person: I will not sell you this spaghetti, you have to take it over my cold dead hands", "You know there is only one solution, you have to take it by force!\n\nYou can fight the person by using the following commands:\npunch\ndodge")) {

  private var fightActive: Boolean = false

  override def canBeStarted: Boolean = this.player.has("cash")

  override def interact: String = if (this.canBeStarted) {
    this.fightActive = true
    this.player.location.canMove = false
    super.interact
  } else "nothing interesting to interact with"

  //checks winning conditions for fight
  override def turnCheck(): String = if (bossHealth <= 0) {
    this.game.activeEvent = None
    eventDone = true
    this.player.location.canMove = true
    this.player.addItem(new Item("spaghetti", "uummmm, smells nice"))
    this.fightActive = false
    this.player.destoryItem("cash")
    "Congratulations you have beaten the person and got the spaghetti!"
  } else if (youHealth <= 0) {
    this.game.activeEvent = None
    this.fightActive = false
    this.game.isLost = true
    this.player.destoryItem("cash")
    "You have been beaten."
  } else ""

  //initial health
  var bossHealth = 100
  var youHealth = 100
  //damage
  val youDamage = 15
  val bossDamage = 10


  override def fight(action: String): String = {
    if (fightActive) {
    var output = ""
      action match {
        case "punch" => {
          if (Random.nextInt(2) == 0) {
            bossHealth = Math.max(0, bossHealth - youDamage)
            output = "you hit"
          } else {
            youHealth = Math.max(0, youHealth - bossDamage)
            output = "you swing and miss and get hit"
          }
        }
        case "dodge" => {
          output = "you dodge the incoming punch"
        }
        case "run" => {
          youHealth -= bossDamage
          output = "you try to run away but get hit in the process"
        }
        case _ => output = "you can't do that"
      }
      output + s"\n\nBoss Health: ${bossHealth}\nYour health: ${youHealth}"
    } else "unknown command: " + action
    }
}
