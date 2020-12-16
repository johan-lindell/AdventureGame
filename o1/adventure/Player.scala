package o1.adventure

import scala.collection.mutable.Map


/** A `Player` object represents a player character controlled by the real-life user of the program.
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area, game: Adventure) {
  private val onlyOneCarry = Vector("chicken","mechanical engineer","jacket")
  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private var items = Map[String, Item](/*"mysterious key" -> new Item("mysterious key", "TEST"), "timemachine" -> new TimeMachine(this.game), "cash" -> new Item("cash", "TEST")*/)

  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven

  //returns items
  def getItems = this.items

  /** Returns the current location of the player. */
  def location = this.currentLocation

  //interacts with the current location
  def interact = currentLocation.interact

  //event specific
  def nextLine: String = currentLocation.event match {
    case Some(event) => event.nextLine
    case None => ""
  }
  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player's current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION."
   * also checks area allows player to move*/
  def go(direction: String) = {
    if (this.location.canMove) {
      val destination = this.location.neighbor(direction)
      this.currentLocation = destination.getOrElse(this.currentLocation)
      if (destination.isDefined) "You go to " + direction + "." else "You can't go to " + direction + "."
    } else {
      "can't go there right now"
    }
  }

  def addItem(item: Item) = this.items += item.name -> item
  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }

  def has(itemName: String): Boolean = this.items.keys.toVector.contains(itemName)

  def inventory: String = {
    var str = ""
    if (this.items.nonEmpty) {
      str += "You are carrying:"
      for (key <- this.items.keys) {
        str += s"\n${key}"
      }
    } else {
      str += "You are empty-handed."
    }
    str
  }

  //player picks up item
  def get(itemName: String): String = {
    if (onlyOneCarry.contains(itemName) && onlyOneCarry.exists(item => items.keys.toVector.contains(item))) {
      "You cannot carry that item with another item in you inventory"
    } else {
      val item: Option[Item] = this.location.removeItem(itemName)
      item.foreach(item => this.items += item.name -> item)
      if (item.isDefined) {
        s"You pick up the ${itemName}."
      } else {
        s"There is no ${itemName} here to pick up."
      }
    }
  }

  //player uses item
  def use(itemName: String): String = {
    if (this.has(itemName)) {
      this.items(itemName).use()
    } else {
      "you dont have that item"
    }
  }

  //player examines item
  def examine(itemName: String): String = {
    if (this.has(itemName)) {
      s"You look closely at the ${itemName}.\n${this.items(itemName).description}"
    } else {
      "If you want to examine something, you need to pick it up first."
    }
  }

  //player drops item, item gets added to the current area
  def drop(itemName: String): String = {
    if (this.has(itemName)) {
      this.location.addItem(this.items(itemName))
      this.items = this.items.-(itemName)
      s"You drop the ${itemName}."
    } else {
      "You don't have that!"
    }
  }

  //deletes item form inventory
  def destoryItem(itemName: String) = {
    this.items -= itemName
  }

  //when event has a puzzle use guess command
  def guess(guessed: String): String = currentLocation.event match {
    case Some(event) => event.guess(guessed)
    case None => "Unknown command"
  }

  def lookAround: String = "You look around and see:\n" + this.location.anotherDescription
  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name

  def fight(action: String): String = this.game.activeEvent.map(_.fight(action)).getOrElse("Unknown command: " + action)


  def help: String = {
    "--------HELP--------\n\nCOMMANDS:\n-go: makes the player move to a neighboring area (example: go jmt1).\n"+
    "-quit: you quit the game, game over.\ninventory: lists all items currently in players inventory.\n"+
    "-get: player picks up item that is in the same area as the player\n"+
    "-examine: gives short description of item (example: examine cog1).\n"+
    "-drop: player drops item in the current location\n"+
    "-interact: player interacts with area that player is in\n"+
    "-n: moves the dialogue forward\n"+
    "-use: uses item (example: use cog1).\n"+
    "-guess: makes a guess in a riddle event (example: guess password).\n"+
    "-help: lists all commands, gives hints.\n\n"+
    "AIM OF THE GAME:\nYou travel around otaniemi in search of your keys, when you find your home keys return home(otaranta)."
  }
}


