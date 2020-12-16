package o1.adventure

/** The class `Item` represents items in a text adventure game. Each item has a name
  * and a  *  longer description. (In later versions of the adventure game, items may
  * have other features as well.)
  *
  * N.B. It is assumed, but not enforced by this class, that items have unique names.
  * That is, no two items in a game world have the same name.
  *
  * @param name         the item's name
  * @param description  the item's description */
 class Item(val name: String, val description: String) {

  /** Returns a short textual representation of the item (its name, that is). */
  override def toString = this.name

  def use(): String = "You cant use this item"

}

class Keys(name: String, description: String, val player: Player, val game: Adventure) extends Item(name, description) {
  override def use(): String = {
    if (player.location == game.home) {
      "Win condition (to be edited)"
    } else {
      "you cant use that key here"
    }
  }
}

class Cog(name: String, description: String, val player: Player, amountToCraft: Int, val game: Adventure) extends Item(name, description) {
  override def use(): String = {
    println(player.getItems.keys.count(_.init == this.name.init))
    if (player.getItems.keys.count(_.init == this.name.init) >= amountToCraft) {
      this.player.addItem(new TimeMachine(game))
      for (i <- 1 to amountToCraft) this.player.destoryItem(name.init + i)
      "With blood, sweat and prujut you have crafted a working timemachine"
    } else {
      "if i had more cogs maybe i could craft something"
    }
  }
}

class TimeMachine(val game: Adventure) extends Item("timemachine", "With this you can travel back in time.") {
  override def use(): String = {
    this.game.player.destoryItem("timemachine")
    game.turnCount = 0
    s"Whoa its ${game.time} the timemachine worked!\nUnfortanetly it broke in the process."
  }
}
