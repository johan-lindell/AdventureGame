package o1.adventure

/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of "hard-coded" information which pertain to a very
  * specific adventure game that involves a small trip through a twisted forest. All newly created
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure
  * games, you will need to modify or replace the source code of this class. */
class Adventure {

  /** The title of the adventure game. */
  val title = "The life of a Ikiteekkari"

  private val jmt1 = new Area("Jämeräntaival 1", "", None)
  /** The character that the player controls in the game. */
  val player: Player = new Player(jmt1, this)

  private val smokki      = new Area("Smökki", "Ah, smökki. You reminisce about the memories you don't have about this place.", Some(new Smokki(this.player, this)))
  private val tf = new Area("Teknologföreningen", "Täffä is serving spaghetti today.", Some(new FightEvent(this.player, this)))

  private val kandilafka        = new Area("Kandilafka", "Smells like käsidesi and coffee.", None)
  val ossinlampi    = new Area("Ossinlampi", "A nice peaceful lake.", Some(new Ossinlampi(this.player, this, kandilafka)))
  private val otaranta      = new Area("Otaranta", "You live here.", Some(new HomeEvent("iv", this.player, this)))

  kandilafka.setNeighbors(Vector("ossinlampi" -> ossinlampi, "tf" -> tf))
  tf.setNeighbors(Vector("jmt1" -> jmt1, "ossinlampi" -> ossinlampi, "kandilafka" -> kandilafka, "otaranta" -> otaranta))
  jmt1.setNeighbors(Vector("smökki" -> smokki,"otaranta" -> otaranta, "tf" -> tf, "ossinlampi" -> ossinlampi))
  ossinlampi.setNeighbors(Vector("smökki" -> smokki, "jmt1" -> jmt1, "tf" -> tf, "kandilafka" -> kandilafka))
  otaranta.setNeighbors(Vector("jmt1" -> jmt1))
  smokki.setNeighbors(Vector("tf" -> tf, "ossinlampi" -> ossinlampi))

  var activeEvent: Option[Event] = None

  val home = otaranta

  //items for timemachine
  jmt1.addItem(new Cog("cog1", "It's a regular cogwheel, maybe if i had more I could craft something?",this.player , 3, this))
  otaranta.addItem(new Cog("cog2", "It's a regular cogwheel, maybe if i had more I could craft something?",this.player , 3, this))
  kandilafka.addItem(new Cog("cog3", "It's a regular cogwheel, maybe if i had more I could craft something?",this.player , 3, this))

  //items for ossinlampi puzzle
  ossinlampi.addItem(new Item("chicken", "It's a regular chicken"))
  ossinlampi.addItem(new Item("mechanical engineer", "Koneeeeeee!"))
  ossinlampi.addItem(new Item("jacket", "Esa's jacket"))



  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  // time goes from 14:00-24:00(deadline)
  val timeLimit = 60

  val startTime = 14

  //function that calculates current time
  def time(fturnCount: Int, ftimeLimit: Int): String = {
    val totalMinutes = fturnCount.toDouble / ftimeLimit.toDouble * 10.0 * 60.0
    val hours = (totalMinutes / 60.0).floor.toInt + startTime
    val minutes = (totalMinutes % 60).toInt
    if (minutes.toString.length == 1) {
      s"${hours}:0${minutes}"
    } else {
      s"${hours}:${minutes}"
    }
  }

  def time: String = this.time(this.turnCount, this.timeLimit)

  //last event changes isWon to true if player completes it in time
  var isWon = false

  var isLost = false
  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = isWon

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit || this.isLost

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "You wake up... \n Your head is throbbing...\n\n Ugh... what happened last night? \n Oh peeveli, its wednesday and you have a deadline at 24:00, " +
    "if you don't submit it you will fail your studies." + "\nIn this game you must get to your appartement in Otaranta, you have lost your keys so you must travel around otaniemi and try to find them."


  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete)
      "You succesfully submitted your assignment!\nCongratulations you won!"
    else if (this.turnCount == this.timeLimit)
      "Time is out.\nGame over!"
    else if (this.isLost)
      "Game over!"
    else  // game over due to player quitting
      "Quitter!"
  }


  /** Plays a turn by executing the given in-game command, such as "go west". Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String) = {
    val action = new Action(command)
    var outcomeReport = action.execute(this.player)
    if (outcomeReport.isDefined || !outcomeReport.contains("")) {
      this.turnCount += 1
    }
    //Event starts event if event can be started
    if (this.activeEvent.isEmpty && this.player.location.event.exists(_.canBeStarted == true)) {
      val action = new Action("interact")
      outcomeReport = action.execute(this.player)
    }
    val eventOutput: String = this.activeEvent.map(_.turnCheck()).getOrElse(" ")
    val output = outcomeReport.getOrElse("Unknown command: \"" + command + "\".")
    output + "\n" + eventOutput

  }



}

