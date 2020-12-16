package o1.adventure

import javax.management.Descriptor

import scala.collection.mutable.Map

/** The class `Area` represents locations in a text adventure game world. A game world
  * consists of areas. In general, an "area" can be pretty much anything: a room, a building,
  * an acre of forest, or something completely different. What different areas have in
  * common is that players can be located in them and that they can have exits leading to
  * other, neighboring areas. An area also has a name and a description.
  * @param name         the name of the area
  * @param description  a basic description of the area (typically not including information about items) */
class Area(var name: String, var description: String, val event: Option[Event]) {

  private val neighbors = Map[String, Area]()

  private var items = Map[String, Item]()

  var canMove = true

  def interact: String = {
    event match {
      case Some(event) => if (event.canBeStarted) {
        event.interact
      } else "nothing interesting to interact with"
      case _ => "nothing interesting to interact with"
    }
  }

  var anotherDescription = this.description

  /** Returns the area that can be reached from this area by moving in the given direction. The result
    * is returned in an `Option`; `None` is returned if there is no exit in the given direction. */
  def neighbor(direction: String) = this.neighbors.get(direction)


  /** Adds an exit from this area to the given area. The neighboring area is reached by moving in
    * the specified direction from this area. */
  def setNeighbor(direction: String, neighbor: Area) = {
    this.neighbors += direction -> neighbor
  }

  /** Adds exits from this area to the given areas. Calling this method is equivalent to calling
    * the `setNeighbor` method on each of the given direction--area pairs.
    * @param exits  contains pairs consisting of a direction and the neighboring area in that direction
    * @see [[setNeighbor]] */
  def setNeighbors(exits: Vector[(String, Area)]) = {
    this.neighbors ++= exits
  }


  /** Returns a multi-line description of the area as a player sees it. This includes a basic
    * description of the area as well as information about exits and items. The return
    * value has the form "DESCRIPTION\n\nExits available: DIRECTIONS SEPARATED BY SPACES".
    * The directions are listed in an arbitrary order. */
  def fullDescription = {
    val exitList = "\n\nExits available: " + this.neighbors.keys.mkString(" ")
    var itemDescription = ""
    if (this.items.nonEmpty) itemDescription = "\nYou see here: " + this.items.keys.mkString(" ")
    this.description + itemDescription + exitList
  }

  def contains(itemName: String): Boolean = items.contains(itemName)

  def removeItem(itemName: String): Option[Item] = {
    if (this.contains(itemName)) {
      val item = this.items.get(itemName)
      this.items = this.items.-(itemName)
      item
    } else None
  }

  def addItem(item: Item): Unit = {
    this.items += item.name -> item
  }

  /** Returns a single-line description of the area for debugging purposes. */
  override def toString = this.name + ": " + this.description.replaceAll("\n", " ").take(150)

}

