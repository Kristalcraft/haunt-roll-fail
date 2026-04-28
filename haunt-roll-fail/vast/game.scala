// FILE: vast/game.scala
// VERSION: 0.1.7
// START_MODULE_CONTRACT
// PURPOSE: Implement the authoritative Vast game state machine, including setup, action resolution, state validation, and game progression.
// SCOPE: Own runtime state, apply Actions to produce Continue values, maintain board/faction invariants, and emit player-facing logs.
// DEPENDS: vast.meta, vast.styles, hrf.base, hrf.logger, hrf.ui
// LINKS: M-VAST-GAME, M-VAST-META, M-VAST-RULES, M-VAST-UI, M-VAST-SERIALIZE
// ROLE: RUNTIME
// MAP_MODE: EXPORTS
// END_MODULE_CONTRACT
//
// START_MODULE_MAP
// Game - authoritative Vast runtime state and action-resolution engine
// END_MODULE_MAP
//
// START_CHANGE_SUMMARY
// LAST_CHANGE: v0.1.7 - Cave tail split: `game-cave-tail.scala` (`GameCaveTailSupport`), `game-board-tail.scala` (`GameBoardTailSupport`), thief bridge cases in `game-thief` (`thiefCaveTailDispatchPart3`). v0.1.6: Goblins. v0.1.5: Dragon/`caveTailPart3`.
// END_CHANGE_SUMMARY
package vast
//
//
//
//
import hrf.colmat._
import hrf.logger._
//
//
//
//

import hrf.tracker2._
import hrf.elem._

import vast.elem._


trait AttackTarget

trait Faction extends BasePlayer with AttackTarget with Styling with Elementary {
    def name = toString
    def short = name.take(1)
    def style = short.toLowerCase
    def ss = name.take(1).styled(this)
    def elem : Elem = name.styled(this).styled(styles.condensed)
}

case object Knight extends Faction
case object Goblins extends Faction
case object Dragon extends Faction
case object Cave extends Faction
case object Thief extends Faction


case class Absolute(x : Int, y : Int) extends Record {
    def add(b : Bearing) = Absolute(x + b.dx, y + b.dy)
    def add(dx : Int, dy : Int) = Absolute(x + dx, y + dy)
    def dist(d : Absolute) = (x - d.x).abs + (y - d.y).abs
}

case class Relative(x : Int, y : Int) extends Record {
    def add(b : Bearing) = Relative(x + b.dx, y + b.dy)
    def add(dx : Int, dy : Int) = Relative(x + dx, y + dy)
    def dist(d : Relative) = (x - d.x).abs + (y - d.y).abs
}

case class Box(xl : Int, yl : Int, xh : Int, yh : Int)

abstract class Bearing(val rotation : Int, val dx : Int, val dy : Int) extends Record with Elementary with Styling {
    def elem = toString.hh
    val reverse : Bearing
}

case object North extends Bearing(0, 0, -1) { val reverse = South }
case object East extends Bearing(1, 1, 0) { val reverse = West }
case object South extends Bearing(2, 0, 1) { val reverse = North }
case object West extends Bearing(3, -1, 0) { val reverse = East }

object Bearings {
    val nswe = $[Bearing](North, South, West, East)
    val wnes = $[Bearing](West, North, East, South)
    val all = $[Bearing](North, East, South, West)
}

case class Walls(north : Boolean, east : Boolean, south : Boolean, west : Boolean) extends Record {
    def any = north || east || south || west

    def wall(d : Bearing) : Boolean = d @@ {
        case North => north
        case East => east
        case South => south
        case West => west
    }

    def rotate(r : Bearing) = r @@ {
        case North => this
        case East => Walls(west, north, east, south)
        case South => Walls(south, west, north, east)
        case West => Walls(east, south, west, north)
    }
}

object Walls {
    val Open = Walls(false, false, false, false)
    val Side = Walls(false, false, false, true)
    val Corner = Walls(false, false, true, true)
    val Tunnel = Walls(false, true, false, true)
    val Deadend = Walls(false, true, true, true)
}

case class Tile(walls : Walls, name : String) extends Record


trait Cell


case object Pending extends Cell
case object Revealing extends Cell
case class HiddenTile(tile : Tile, tribe : Tribe, tokens : $[Token]) extends Cell with Record
case class Explored(img : String, walls : Walls, bearing : Bearing, original : |[HiddenTile]) extends Cell
case class Removing(cell : Cell) extends Cell
case class River(id : String, bearing : Bearing) extends Cell
case class Magma(id : String, bearing : Bearing) extends Cell
case class CanyonBrigde(bearing : Bearing) extends Cell
case class CanyonImpassable(bearing : Bearing) extends Cell
case object Pit extends Cell
case object Mushrooms extends Cell
case object Emptiness extends Cell


object Tiles {
    val ambushes =
        HiddenTile(Tile(Walls.Open   , "empty-open-1"   ), Fangs, $(Ambush)) ::
        HiddenTile(Tile(Walls.Tunnel , "empty-tunnel-1" ), Bones, $(Ambush)) ::
        HiddenTile(Tile(Walls.Side   , "empty-side-1"   ), Eye  , $(Ambush)) ::
        HiddenTile(Tile(Walls.Side   , "empty-side-2"   ), Fangs, $(Ambush)) ::
        HiddenTile(Tile(Walls.Open   , "empty-open-2"   ), Bones, $(Ambush)) ::
        HiddenTile(Tile(Walls.Side   , "empty-side-3"   ), Eye  , $(Ambush)) ::
        HiddenTile(Tile(Walls.Corner , "empty-corner-1" ), Fangs, $(Ambush)) ::
        HiddenTile(Tile(Walls.Tunnel , "empty-tunnel-2" ), Bones, $(Ambush)) ::
        HiddenTile(Tile(Walls.Tunnel , "empty-tunnel-3" ), Eye  , $(Ambush)) ::
        HiddenTile(Tile(Walls.Open   , "empty-open-3"   ), Fangs, $(Ambush)) ::
        HiddenTile(Tile(Walls.Corner , "empty-corner-2" ), Bones, $(Ambush)) ::
        HiddenTile(Tile(Walls.Deadend, "empty-deadend-1"), Eye  , $(Ambush)) ::
        HiddenTile(Tile(Walls.Open   , "empty-open-4"   ), Fangs, $(Ambush)) ::
        HiddenTile(Tile(Walls.Corner , "empty-corner-3" ), Bones, $(Ambush)) ::
        HiddenTile(Tile(Walls.Corner , "empty-corner-4" ), Eye  , $(Ambush)) ::
    $

    val events =
        HiddenTile(Tile(Walls.Tunnel , "empty-tunnel-4" ), Fangs, $(Event)) ::
        HiddenTile(Tile(Walls.Side   , "empty-side-4"   ), Bones, $(Event)) ::
        HiddenTile(Tile(Walls.Tunnel , "empty-tunnel-5" ), Eye  , $(Event)) ::
        HiddenTile(Tile(Walls.Tunnel , "empty-tunnel-6" ), Fangs, $(Event)) ::
        HiddenTile(Tile(Walls.Open   , "empty-open-5"   ), Bones, $(Event)) ::
        HiddenTile(Tile(Walls.Side   , "empty-side-5"   ), Eye  , $(Event)) ::
        HiddenTile(Tile(Walls.Open   , "empty-open-6"   ), Fangs, $(Event)) ::
        HiddenTile(Tile(Walls.Corner , "empty-corner-5" ), Bones, $(Event)) ::
        HiddenTile(Tile(Walls.Corner , "empty-corner-6" ), Eye  , $(Event)) ::
        HiddenTile(Tile(Walls.Open   , "empty-open-7"   ), Fangs, $(Event)) ::
        HiddenTile(Tile(Walls.Corner , "empty-corner-7" ), Bones, $(Event)) ::
        HiddenTile(Tile(Walls.Deadend, "empty-deadend-2"), Eye  , $(Event)) ::
        HiddenTile(Tile(Walls.Side   , "empty-side-6"   ), Fangs, $(Event)) ::
        HiddenTile(Tile(Walls.Open   , "empty-open-b"   ), Bones, $(Event)) ::
        HiddenTile(Tile(Walls.Open   , "empty-open-c"   ), Eye  , $(Event)) ::
    $

    val treasures =
        HiddenTile(Tile(Walls.Open   , "empty-open-8"   ), Fangs, $(Chest)) ::
        HiddenTile(Tile(Walls.Open   , "empty-open-9"   ), Bones, $(Chest)) ::
        HiddenTile(Tile(Walls.Open   , "empty-open-a"   ), Eye  , $(Chest)) ::
        HiddenTile(Tile(Walls.Side   , "empty-side-7"   ), Fangs, $(Chest)) ::
        HiddenTile(Tile(Walls.Tunnel , "empty-tunnel-7" ), Bones, $(Chest)) ::
        HiddenTile(Tile(Walls.Corner , "empty-corner-8" ), Eye  , $(Chest)) ::
   $

    val vaults =
        HiddenTile(Tile(Walls.Corner , "vault-corner-4" ), Fangs, $(Vault)) ::
        HiddenTile(Tile(Walls.Corner , "vault-corner-4" ), Bones, $(Vault)) ::
        HiddenTile(Tile(Walls.Side   , "vault-side-8"   ), Eye  , $(Vault)) ::
        HiddenTile(Tile(Walls.Corner , "vault-corner-4" ), Fangs, $(Vault)) ::
        HiddenTile(Tile(Walls.Side   , "vault-side-9"   ), Bones, $(Vault)) ::
        HiddenTile(Tile(Walls.Side   , "vault-side-a"   ), Eye  , $(Vault)) ::
    $

    val crystals =
        HiddenTile(Tile(Walls.Open   , "crystal-open-1"   ), Fangs, $(Crystal)) ::
        HiddenTile(Tile(Walls.Side   , "crystal-side-1"   ), Bones, $(Crystal)) ::
        HiddenTile(Tile(Walls.Corner , "crystal-corner-1" ), Eye  , $(Crystal)) ::
        HiddenTile(Tile(Walls.Side   , "crystal-side-2"   ), Fangs, $(Crystal)) ::
        HiddenTile(Tile(Walls.Corner , "crystal-corner-2" ), Bones, $(Crystal)) ::
        HiddenTile(Tile(Walls.Open   , "crystal-open-2"   ), Eye  , $(Crystal)) ::
        HiddenTile(Tile(Walls.Corner , "crystal-corner-3" ), Fangs, $(Crystal)) ::
        HiddenTile(Tile(Walls.Open   , "crystal-open-3"   ), Bones, $(Crystal)) ::
        HiddenTile(Tile(Walls.Side   , "crystal-side-3"   ), Eye  , $(Crystal)) ::
    $
}



trait Token extends Record with Elementary {
    def elem : Elem = ("[" + toString + "]").hl
}

case object Entrance extends Token
case object HeroCube extends Token
case object Chest extends Token {
    override def elem = "Treasure".hl
}
case class DragonGem(faction : Dragon.type, power : Power) extends Token with Elementary {
    override def elem = power.elem ~ " " ~ "Dragon Gem".styled(Dragon)
}
case object Ambush extends Token with Elementary {
    override def elem = "Ambush".styled(Goblins)
}
case object Event extends Token with Elementary {
    override def elem = "Event".hl
}
case object Crystal extends Token with AttackTarget with Elementary {
    override def elem = "Crystal".styled(styles.crystal)
}
case object BrokenCrystal extends Token
case object Vault extends Token
case class FlameWall(f : Dragon.type) extends Token


case class TokenAt(token : Token, position : Relative) extends AttackTarget with Record with Elementary {
    def elem = token.elem
}


abstract class SideQuest(val name : String, val id : String, val grit : Int) extends Record with Elementary {
    def elem = name.styled(Knight)
    def elemLog = (hrf.elem.FigureSpace + name + hrf.elem.FigureSpace).styled(xlo.pre, xstyles.outlined, styles.get(Knight))
}

case object Adventurous extends SideQuest("Adventurous", "adventurous", 6)
case object Bedecked extends SideQuest("Bedecked", "bedecked", 4)
case object Cunning extends SideQuest("Cunning", "cunning-dragon", 3)
case object Daring extends SideQuest("Daring", "daring-goblins-dragon", 4)
case object EagleEyed extends SideQuest("Eagle-Eyed", "eagle-eyed", 3)
case object Fearless extends SideQuest("Fearless", "fearless-goblins", 4)
case object Intrepid extends SideQuest("Intrepid", "intrepid", 3)
case object Persistent extends SideQuest("Persistent", "persistent", 4)
case object StalwartAmbushes extends SideQuest("Stalwart", "stalwart-ambushes", 5)
case object StalwartGoblins extends SideQuest("Stalwart", "stalwart-goblins", 5)
case object Swift extends SideQuest("Swift", "swift", 4)

object SideQuests {
    def questsFor(l : $[Faction]) = $(Adventurous, Bedecked, Cunning, Daring, EagleEyed, Fearless, Intrepid, Persistent, Swift) ++
        l.has(Goblins).$(StalwartGoblins) ++
        l.has(Goblins).not.$(StalwartAmbushes)
}

abstract class Tribe(val faction : Goblins.type) extends Record with Elementary with AttackTarget {
    def name = toString
    def elem = name.styled(Goblins)
}

case object Fangs extends Tribe(Goblins)
case object Bones extends Tribe(Goblins)
case object Eye extends Tribe(Goblins)

abstract class Equipment(val name : String, val id : String) extends Record with Elementary {
    def elem = name.styled(Knight)
    def elemLog = (hrf.elem.FigureSpace + name + hrf.elem.FigureSpace).styled(xlo.pre, xstyles.outlined, styles.get(Knight))
}

abstract class Stat(name : String) extends Equipment(name, "")

case object Movement extends Stat("Movement")
case object Perception extends Stat("Perception")
case object Strength extends Stat("Strength")

case object Shield extends Equipment("Shield", "equipment-shield")
case object Bow extends Equipment("Bow", "equipment-bow")
case object Bomb extends Equipment("Bomb", "equipment-bomb")
case object AncientMap extends Equipment("Ancient Map", "equipment-ancient-map")
case object ElvishSword extends Equipment("Elvish Sword", "treasure-elvish-sword")
case object EnchantedBow extends Equipment("Enchanted Bow", "treasure-enchanted-bow")
case object HeroicBoots extends Equipment("Heroic Boots", "treasure-heroic-boots")
case object Javelin extends Equipment("Javelin", "treasure-javelin")
case object MightyAxe extends Equipment("Mighty Axe", "treasure-mighty-axe")
case object PixieLantern extends Equipment("Pixie Lantern", "treasure-pixie-lantern")
case object PotionKit extends Equipment("Potion Kit", "treasure-potion-kit")

case object SetAside extends Stat("Set Aside")

trait Treasure extends Record with Elementary {
    def id : String
}

case class EquipmentTreasure(equipment : Equipment) extends Treasure {
    def id = equipment.id
    def elem = equipment.elem
}

object Treasures {
    val all = $(ElvishSword, EnchantedBow, HeroicBoots, Javelin, MightyAxe, PixieLantern, PotionKit)./(EquipmentTreasure)
}


abstract class Event(val name : String, val id : String) extends Record with Elementary {
    def elem = name.styled(Knight)
    def elemLog = (hrf.elem.FigureSpace + name + hrf.elem.FigureSpace).styled(xlo.pre, xstyles.outlined, styles.get(Knight))
    def img = Image("event-" + id, styles.card, styles.inline)
}

abstract class Ambush(id : String) extends Event("Ambush", id)
case object Ambush1 extends Ambush("ambush-1")
case object Ambush2 extends Ambush("ambush-2")
case object Ambush3 extends Ambush("ambush-3")
case object Ambush4 extends Ambush("ambush-4")
abstract class DeepAndDarkCave(id : String) extends Event("Deep and Dark", id)
case object DeepAndDarkCave1 extends DeepAndDarkCave("deep-and-dark-cave-1")
case object DeepAndDarkCave2 extends DeepAndDarkCave("deep-and-dark-cave-2")
case object DeepAndDarkCave3 extends DeepAndDarkCave("deep-and-dark-cave-3")
abstract class DeepAndDarkKnight(id : String) extends Event("Deep and Dark", id)
case object DeepAndDarkKnight1 extends DeepAndDarkKnight("deep-and-dark-knight-1")
case object DeepAndDarkKnight2 extends DeepAndDarkKnight("deep-and-dark-knight-2")
case object DeepAndDarkKnight3 extends DeepAndDarkKnight("deep-and-dark-knight-3")
abstract class Rats(id : String) extends Event("Rats", id)
case object Rats1 extends Rats("rats")
case object Rats2 extends Rats("rats")
case object Rats3 extends Rats("rats")
case object CaveBread extends Event("Cave Bread", "cave-bread")
trait Fresh extends Event
case object FreshAir extends Event("Fresh Air", "fresh-air") with Fresh
case object FreshWater extends Event("Fresh Water", "fresh-water") with Fresh
case object Light extends Event("Light", "light")
case object VantagePoint extends Event("Vantage Point", "vantage-point")


object Events {
    def eventsFor(l : $[Faction]) =
        $(Ambush1, Ambush2, Ambush3, Ambush4, Rats1, Rats2, Rats3, CaveBread, FreshAir, FreshWater, Light, VantagePoint) ++
        l.has(Cave).$(DeepAndDarkCave1, DeepAndDarkCave2, DeepAndDarkCave3) ++
        l.has(Cave).not.$(DeepAndDarkKnight1, DeepAndDarkKnight2, DeepAndDarkKnight3)
}

sealed trait Pattern extends Record with Elementary {
    def elem = toString.styled(Dragon)
}


case object Around extends Pattern
case object Center extends Pattern
case object Diamond extends Pattern
case object Cross extends Pattern
case object Corners extends Pattern
case object Saltire extends Pattern
case object FullSquare extends Pattern

object Pattern {
    val die = $(Around, Center, Diamond, Cross, Corners, Saltire)
}

class Board {
    var width : Int = 1
    var height : Int = 1

    var center : Absolute = Absolute(0, 0)
    var entrance : Relative = Relative(0, 0)

    var cells : Array[Array[Cell]] = Array.fill(1, 1)(Explored("entrance", Walls.Open, North, None))
    var tokens : Array[Array[$[Token]]] = Array.fill(1, 1)($(Entrance))
    var letters : Map[Faction, Array[Array[|[String]]]] = Map()

    val lettering : $[String] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toArray.$.use(l => l./("" + _) ++ l./(c => "" + c + c))

    var bombed : $[(Relative, Bearing)] = $
    var rockslides : $[(Relative, Bearing)] = $

    var inner : $[Relative] = $()
    var outer : $[Relative] = $(Relative(0, 0))

    def pattern(c : Absolute, p : Pattern) : $[Absolute] = {
        (-1).to(1)./~(j => (-1).to(1)./~(i => p @@ {
            case FullSquare =>                      Some(Absolute(c.x + i, c.y + j))
            case Around  if (i != 0) || (j != 0) => Some(Absolute(c.x + i, c.y + j))
            case Center  if (i == 0) && (j == 0) => Some(Absolute(c.x + i, c.y + j))
            case Diamond if (i == 0) != (j == 0) => Some(Absolute(c.x + i, c.y + j))
            case Cross   if (i == 0) || (j == 0) => Some(Absolute(c.x + i, c.y + j))
            case Corners if (i != 0) && (j != 0) => Some(Absolute(c.x + i, c.y + j))
            case Saltire if (i != 0) == (j != 0) => Some(Absolute(c.x + i, c.y + j))
            case _ => None
        }))
    }

    def pattern(c : Relative, p : Pattern) : $[Relative] = {
        (-1).to(1)./~(j => (-1).to(1)./~(i => p @@ {
            case FullSquare =>                      Some(Relative(c.x + i, c.y + j))
            case Around  if (i != 0) || (j != 0) => Some(Relative(c.x + i, c.y + j))
            case Center  if (i == 0) && (j == 0) => Some(Relative(c.x + i, c.y + j))
            case Diamond if (i == 0) != (j == 0) => Some(Relative(c.x + i, c.y + j))
            case Cross   if (i == 0) || (j == 0) => Some(Relative(c.x + i, c.y + j))
            case Corners if (i != 0) && (j != 0) => Some(Relative(c.x + i, c.y + j))
            case Saltire if (i != 0) == (j != 0) => Some(Relative(c.x + i, c.y + j))
            case _ => None
        }))
    }

    def valid(p : Relative) : Boolean = {
        val x = p.x + center.x
        val y = p.y + center.y

        x >= 0 && x < width && y >= 0 && y < height
    }

    def get(p : Relative) : Cell = {
        val x = p.x + center.x
        val y = p.y + center.y

        if (x >= 0 && x < width && y >= 0 && y < height)
            cells(x)(y)
        else
            Emptiness
    }
    def set(p : Relative, c : Cell) { cells(p.x + center.x)(p.y + center.y) = c }

    def list(p : Relative) : $[Token] = {
        val x = p.x + center.x
        val y = p.y + center.y

        if (x >= 0 && x < width && y >= 0 && y < height)
            tokens(x)(y)
        else
            $
    }
    def place(p : Relative, t : Token) { tokens(p.x + center.x)(p.y + center.y) = tokens(p.x + center.x)(p.y + center.y) :+ t }
    def remove(p : Relative, t : Token) { tokens(p.x + center.x)(p.y + center.y) = tokens(p.x + center.x)(p.y + center.y) :- t }

    def write(f : Faction, p : Relative, s : String) {
        if (letters.contains(f).not)
            if (s.any)
                letters += f -> Array.fill(width, height)(None : |[String])
            else
                return

        letters(f)(p.x + center.x)(p.y + center.y) = s.some
    }

    def writeAll(f : Faction, l : $[Relative]) {
        letters += f -> Array.fill(width, height)(None : |[String])

        var markings = lettering

        l.foreach { p =>
            write(f, p, markings.head)
            markings = markings.drop(1)
        }
    }

    def read(f : Faction, p : Relative) = letters.get(f)./~(_(p.x + center.x)(p.y + center.y))

    def accessible : Array[Array[Boolean]] = {
        val mapping = Array.fill(width, height)(false)

        mapping(center.x + entrance.x)(center.y + entrance.y) = true

        var expanding = true
        while (expanding) {
            expanding = false

            Bearings.all.foreach { dir =>
                1.until(width - 1).foreach { i =>
                    1.until(height - 1).foreach { j =>
                        if (mapping(i)(j))
                            if (mapping(i + dir.dx)(j + dir.dy).not)
                                if (cells(i + dir.dx)(j + dir.dy).is[Explored])
                                    if (wall(Relative(i - center.x, j - center.y), dir).not) {
                                       mapping(i + dir.dx)(j + dir.dy) = true
                                       expanding = true
                                   }
                    }
                }
            }
        }

        mapping
    }

    def wall(p : Relative, d : Bearing) : Boolean = {
        if (rockslides.has((p, d)) || rockslides.has((p.add(d), d.reverse)))
            return true

        if (bombed.has((p, d)) || bombed.has((p.add(d), d.reverse)))
            return false

        val a = get(p)
        val b = get(p.add(d))

        val aw = a.as[Explored].?(_.walls.wall(d))
        val bw = b.as[Explored].?(_.walls.wall(d.reverse))

        aw || bw
    }

    def visible(a : Relative, b : Relative) : Boolean = {
        if (a == b)
            true
        else
        if (a.x == b.x) {
            val x = a.x
            val ay = min(a.y, b.y)
            val by = max(a.y, b.y)

            (ay + 1).to(by - 1).forall(y => get(Relative(x, y)).use(c => c != Emptiness && c.is[HiddenTile].not)) && ay.until(by).forall(y => wall(Relative(x, y), South).not)
        }
        else
        if (a.y == b.y) {
            val y = a.y
            val ax = min(a.x, b.x)
            val bx = max(a.x, b.x)

            (ax + 1).to(bx - 1).forall(x => get(Relative(x, y)).use(c => c != Emptiness && c.is[HiddenTile].not)) && ax.until(bx).forall(x => wall(Relative(x, y), East).not)
        }
        else
            false
    }

    def move(l : $[Absolute], dx : Int, dy : Int) {
        val ml = l./(p => Absolute(p.x + dx, p.y + dy))
        val nw = width
        val nh = height

        val ll = letters.keys.$

        val nc = Array.fill(nw, nh)(Emptiness : Cell)
        val nt = Array.fill(nw, nh)(Nil : $[Token])
        val nl = ll./(f => f -> Array.fill(nw, nh)(None : |[String])).toMap

        0.until(width).foreach { i =>
            0.until(height).foreach { j =>
                val p = Absolute(i, j)
                if (ml.has(p)) {
                    nc(i)(j) = cells(i - dx)(j - dy)
                    nt(i)(j) = tokens(i - dx)(j - dy)
                    ll.foreach { f =>
                        nl(f)(i)(j) = letters(f)(i - dx)(j - dy)
                    }
                }
                else
                if (l.has(p)) {
                    nc(i)(j) = Emptiness
                    nt(i)(j) = $
                    ll.foreach { f =>
                        nl(f)(i)(j) = None
                    }
                }
                else {
                    nc(i)(j) = cells(i)(j)
                    nt(i)(j) = tokens(i)(j)
                    ll.foreach { f =>
                        nl(f)(i)(j) = letters(f)(i)(j)
                    }
                }
            }
        }

        bombed = bombed./{ case (p, dir) =>
            l.has(Absolute(p.x + center.x, p.y + center.y)).?((Relative(p.x + dx, p.y + dy), dir)).|((p, dir))
        }

        rockslides = rockslides./{ case (p, dir) =>
            l.has(Absolute(p.x + center.x, p.y + center.y)).?((Relative(p.x + dx, p.y + dy), dir)).|((p, dir))
        }

        cells = nc
        tokens = nt
        letters = nl
    }

    def squeeze(l : $[Relative]) {
        l.foreach { p =>
            if (get(p) == Emptiness)
                set(p, Pending)
        }

        val up    = max(0, 0.until(height)        .takeWhile(j => 0.until(width ).forall(i => cells(i)(j) == Emptiness)).num - 1)
        val down  = max(0, 0.until(height).reverse.takeWhile(j => 0.until(width ).forall(i => cells(i)(j) == Emptiness)).num - 1)
        val left  = max(0, 0.until(width )        .takeWhile(i => 0.until(height).forall(j => cells(i)(j) == Emptiness)).num - 1)
        val right = max(0, 0.until(width ).reverse.takeWhile(i => 0.until(height).forall(j => cells(i)(j) == Emptiness)).num - 1)

        if (up + down + left + right > 0) {
            val nw = width - left - right
            val nh = height - up - down

            val ll = letters.keys.$

            val nc = Array.fill(nw, nh)(Emptiness : Cell)
            val nt = Array.fill(nw, nh)(Nil : $[Token])
            val nl = ll./(f => f -> Array.fill(nw, nh)(None : |[String])).toMap

            0.until(nw).foreach { i =>
                0.until(nh).foreach { j =>
                    nc(i)(j) = cells(i + left)(j + up)
                    nt(i)(j) = tokens(i + left)(j + up)
                    ll.foreach { f =>
                        nl(f)(i)(j) = letters(f)(i + left)(j + up)
                    }
                }
            }

            cells = nc
            tokens = nt
            letters = nl

            center = Absolute(center.x - left, center.y - up)

            width = nw
            height = nh

            inner = 1.until(width - 1)./~(i => 1.until(height - 1)./(j => Relative(i - center.x, j - center.y)))
            outer = 0.until(width - 0)./~(i => 0.until(height - 0)./(j => Relative(i - center.x, j - center.y)))
        }

        l.foreach { p =>
            if (get(p) == Pending)
                set(p, Emptiness)
        }
    }

    def expand() {
        val up    = 0.until(width).exists(i => cells(i)(0) != Emptiness)
        val down  = 0.until(width).exists(i => cells(i)(height - 1) != Emptiness)
        val left  = 0.until(height).exists(j => cells(0)(j) != Emptiness)
        val right = 0.until(height).exists(j => cells(width - 1)(j) != Emptiness)

        if (up || down || left || right) {
            val nw = width + left.??(1) + right.??(1)
            val nh = height + up.??(1) + down.??(1)

            val ll = letters.keys.$

            val nc = Array.fill(nw, nh)(Emptiness : Cell)
            val nt = Array.fill(nw, nh)(Nil : $[Token])
            val nl = ll./(f => f -> Array.fill(nw, nh)(None : |[String])).toMap

            0.until(width).foreach { i =>
                0.until(height).foreach { j =>
                    nc(i + left.??(1))(j + up.??(1)) = cells(i)(j)
                    nt(i + left.??(1))(j + up.??(1)) = tokens(i)(j)
                    ll.foreach { f =>
                        nl(f)(i + left.??(1))(j + up.??(1)) = letters(f)(i)(j)
                    }
                }
            }

            cells = nc
            tokens = nt
            letters = nl

            center = Absolute(center.x + left.??(1), center.y + up.??(1))

            width = nw
            height = nh

            inner = 1.until(width - 1)./~(i => 1.until(height - 1)./(j => Relative(i - center.x, j - center.y)))
            outer = 0.until(width - 0)./~(i => 0.until(height - 0)./(j => Relative(i - center.x, j - center.y)))
        }
    }
}


trait Player {
    def positions : $[Relative]
}

// class AssignedStamina {
//     var movement = 0
//     var perception = 0
//     var strength = 0
//     var bomb = 0
//     var bow = 0
//     var map = 0
//     var shield = 0
//     var sword = 0
//     var axe = 0
//     var ebow = 0
//     var boots = 0
//     var lantern = 0
//     var aside = 0
//
//     def total = movement + perception + strength + bomb + bow + map + shield + sword + axe + ebow + boots + lantern + aside
// }

object Stamina extends Elementary {
    def elem = "Stamina".styled(Knight)
}

class KnightPlayer(val game : Game, val faction : Knight.type) extends Player {
    var position = Relative(0, 0)

    var health = 7

    var grit = 0

    def score(n : Int) {
        val s = stamina
        val g = grit

        var nn = n

        if (nn < -1 && assigned.has(Shield))
            nn = -1

        grit += nn

        if (grit < 0)
            grit = 0

        if (grit > 45)
            grit = 45

        nn = (grit - g)

        if (grit > g) {
            game.log(faction, "gained", nn.hl, "Grit".styled(faction))

            if (grit >=  5 && g <  5) stamina += 1
            if (grit >= 11 && g < 11) stamina += 1
            if (grit >= 18 && g < 18) stamina += 1
            if (grit >= 26 && g < 26) stamina += 1
            if (grit >= 35 && g < 35) stamina += 1

            if (stamina > 7)
                stamina = 7

            if (stamina != s)
                game.log(faction, "gained", (stamina - s).hl, Stamina)
        }

        if (grit < g) {
            game.log(faction, "lost", (-nn).hl, "Grit".styled(faction))

            if (g >=  5 && grit <  5) stamina -= 1
            if (g >= 11 && grit < 11) stamina -= 1
            if (g >= 18 && grit < 18) stamina -= 1
            if (g >= 26 && grit < 26) stamina -= 1
            if (g >= 35 && grit < 35) stamina -= 1

            if (stamina < 2)
                stamina = 2

            if (stamina - lost - poison < 2)
                lost = stamina - poison - 2

            if (stamina != s)
                game.log(faction, "lost", (s - stamina).hl, Stamina)
        }
    }

    var sidequests : $[SideQuest] = $

    def quests = sidequests.take(3)

    var dark : Boolean = false

    var attacked : $[Faction] = $

    val basic : $[Equipment] = $(AncientMap, Shield, Bow, Bomb)

    var stash : $[Equipment] = $

    var arsenal : $[Equipment] = $

    def invertory = stash ++ arsenal ++ basic

    def reveal(q : Equipment) {
        if (stash.has(q)) {
            stash :-= q
            arsenal :+= q
        }
        else
        if (arsenal.has(q).not)
            throw new Error("Equipment " + q + " not found in arsenal or stash to reveal")
    }

    def discard(q : Equipment) {
        if (arsenal.has(q))
            arsenal :-= q
        else
        if (stash.has(q))
            stash :-= q
        else
            throw new Error("Equipment " + q + " not found in arsenal or stash to discard")
    }

    var bombs = 3

    var stamina = 2
    var lost = 0
    var poison = 0

    var assigned : $[Equipment] = $ // new AssignedStamina

    def free = stamina - lost - poison - assigned.num

    def movement = 1 + assigned.count(Movement) * 2 + assigned.count(HeroicBoots) * 4 + assigned.count(PixieLantern)
    def perception = 1 + assigned.count(Perception) + assigned.count(ElvishSword) + assigned.count(PixieLantern)
    def strength = 1 + assigned.count(Strength) + strbonus
    def strbonus = (game.current == faction).??(assigned.count(ElvishSword) + assigned.count(MightyAxe))

    var path : $[Relative] = $
    var encountered : $[Relative] = $

    def moves = path.num
    def encounters = encountered.num

    def positions = $(position)
}


abstract class WarCard(val fangs : Int, val bones : Int, val eye : Int, val monsters : Int, val secrets : Int, val rage : Int) extends Record with Elementary {
    def name = toString
    def growth(t : Tribe) = t @@ {
        case Fangs => fangs
        case Bones => bones
        case Eye => eye
    }
    def elem = name.styled(styles.get(Goblins))
    def elemLog = (hrf.elem.FigureSpace + name + hrf.elem.FigureSpace).styled(xlo.pre, xstyles.outlined, styles.get(Goblins))
}

case object Thirst extends WarCard(3, 2, 1, 1, 0, 0)
case object Spite extends WarCard(2, 1, 2, 0, 1, 0)
case object Consumption extends WarCard(1, 3, 2, 1, 0, 0)
case object Desolation extends WarCard(1, 2, 2, 1, 1, 0)
case object Waste extends WarCard(2, 1, 3, 0, 1, 0)
case object Ruin extends WarCard(2, 2, 1, 1, 0, 0)
case object Hate extends WarCard(2, 2, 2, 0, 0, 0)
case object Fear extends WarCard(1, 1, 1, 1, 2, 0)
case object Desperation extends WarCard(0, 0, 0, 3, 3, 1)
case object Pain extends WarCard(1, 1, 1, 0, 0, 1)

object WarCards {
    val all = $[WarCard](Thirst, Spite, Consumption, Desolation, Waste, Ruin, Hate, Fear, Desperation, Pain)
}

trait Effect extends Elementary

abstract class Secret(val id : String, val name : String) extends Effect with Record with Elementary {
    def elem = name.hl
    def elemLog = (hrf.elem.FigureSpace + name + hrf.elem.FigureSpace).styled(xlo.pre, xstyles.outlined, styles.get(Goblins))
    def img = Image("secret-" + id, styles.card, styles.inline)
}

case object BlindFury extends Secret("blind-fury", "Blind Fury") with Effect
case object CaveIn extends Secret("cave-in", "Cave-In") with Effect
case object FireBomber extends Secret("fire-bomber", "Fire Bomber") with Effect
case object GoblinRuby extends Secret("goblin-ruby", "Goblin Ruby") with Effect
case object Hex extends Secret("hex", "Hex") with Effect
case object HidingSpots extends Secret("hiding-spots", "Hiding Spots") with Effect
case object Leader extends Secret("leader", "Leader")
case object Poison extends Secret("poison", "Poison") with Effect
case object SecretTunnels extends Secret("secret-tunnels", "Secret Tunnels") with Effect
case object Trap extends Secret("trap", "Trap") with Effect

object Secrets {
    val all = $(BlindFury, CaveIn, FireBomber, GoblinRuby, Hex, HidingSpots, Leader, Poison, SecretTunnels, Trap)
}

abstract class Monster(val id : String, val name : String) extends Record with Elementary {
    def elem = name.hl
    def elemLog = (hrf.elem.FigureSpace + name + hrf.elem.FigureSpace).styled(xlo.pre, xstyles.outlined, styles.get(Goblins))
}

case object Blob extends Monster("blob", "Blob")
case object BrightBeetles extends Monster("bright-beetles", "Bright Beetles") with Effect
case object FlameGiant extends Monster("flame-giant", "Flame Giant")
case object Gnome extends Monster("gnome", "Gnome")
case object Golem extends Monster("golem", "Golem")
case object Ogre extends Monster("ogre", "Ogre")
case object PetFrog extends Monster("pet-frog", "Pet Frog")
case object Troll extends Monster("troll", "Troll")
case object Underworm extends Monster("underworm", "Underworm") with Effect
case object Wisp extends Monster("wisp", "Wisp") with Effect

object Monsters {
    val all = $(Blob, BrightBeetles, FlameGiant, Gnome, Golem, Ogre, PetFrog, Troll, Underworm, Wisp)
}


class Village(player : GoblinsPlayer, val tribe : Tribe) {
    val faction = player.faction

    var activated = false
    var halflight = false
    var smashing = false

    var position : |[Relative] = None
    var population = 0
    var monsters : $[Monster] = $
    var effects : $[Effect] = $

    def lurking = position.?(player.game.board.get(_) == Emptiness)
    def hidden = position.none || lurking

    def malaise = player.rage + monsters.count(PetFrog) <= 0
    def bonus = (tribe == Fangs).??(1) + monsters.count(Ogre) + monsters.count(Troll) + monsters.count(FlameGiant) + effects.count(FireBomber) * 2
    def strength = max(0, population + bonus - malaise.??(1))
    def raw = max(0, population + (tribe == Fangs).??(1) - (player.rage <= 0).??(1))
}

class GoblinsPlayer(val game : Game, val faction : Goblins.type) extends Player {
    var rage = 1

    val fangs = new Village(this, Fangs)
    val bones = new Village(this, Bones)
    val eye   = new Village(this, Eye)

    val tribes = $(fangs, bones, eye)

    def tribe(t : Tribe) = tribes.%(_.tribe == t).only

    def adjustRage(d : Int) {
        val n = (rage + d).clamp(0, 3)

        while (n > rage) {
            game.log(faction, "increased", "Rage".styled(styles.hit))
            rage += 1
        }

        while (n < rage) {
            game.log(faction, "decreased", "Rage".styled(styles.hit))
            rage -= 1
        }
    }

    var effects : $[Effect] = $
    var hand : $[Secret] = $
    var revealed : $[Secret] = $
    var ignored : $[Secret] = $
    var used : $[Secret] = $

    object monsters {
        var deck : $[Monster] = $
        var pile : $[Monster] = $
    }

    object secrets {
        var deck : $[Secret] = $
        var pile : $[Secret] = $
    }

    def positions = tribes./~(_.position)
}


trait Power extends Record with Elementary {
    def id : String
    def img : String = "power-" + id
}

case object Claw extends Power {
    def elem = "Claw".styled(Dragon)
    def id : String = "claw"
}

case object Flame extends Power {
    def elem = "Flame".styled(Dragon)
    def id : String = "flame"
}

case object Wing extends Power {
    def elem = "Wing".styled(Dragon)
    def id : String = "wing"
}

trait DragonCard extends Record with Elementary {
    def img = Image(imgid, styles.power)
    def imgs = Image(imgsid, styles.power)
    def imgid : String
    def imgsid : String
}

case class PowerCard(id : String, power : Power) extends DragonCard {
    def imgid = "card-" + power.id
    def imgsid = "power-" + power.id
    def elem = power.elem
}

case class GemCard(power : Power) extends DragonCard {
    def imgid = "gem-" + power.id
    def imgsid = "power-" + power.id
    def elem = power.elem
}



case object FreeMove extends DragonCard {
    def imgid = "free-move"
    def imgsid = "power-wing"
    def elem = "Free Move".hh
}


object DragonCards {
    def basic : $[DragonCard] = 1.to(6)./(i => PowerCard(i.toString, Claw)) ++ 1.to(6)./(i => PowerCard(i.toString, Flame)) ++ 1.to(6)./(i => PowerCard(i.toString, Wing))
}


abstract class Track(val name : String, val min : Int, val max : Int) extends Record with Elementary {
    def elem = name.styled(Dragon)
}

case object Greed extends Track("Greed", 0, 4)
case object Hunger extends Track("Hunger", 0, 4)
case object PrideEvents extends Track("Pride (Events)", 0, 4)
case object PridePlaceGem extends Track("Pride (Place Gem)", 0, 1)
case object PrideDontMove extends Track("Pride (Don't Move)", 0, 1)

class TrackValue(val player : DragonPlayer, val track : Track, alts : $[Track]) {
    var value : Int = track.max
    var reduced : Boolean = false

    def reduce() {
        value -= 1
        reduced = true
    }

    def available = value > 0 && reduced.not && player.tracks.%(t => alts.exists(_ == t.track)).exists(_.reduced).not
}

class DragonPlayer(val game : Game, val faction : Dragon.type) extends Player {
    var health = 5

    var position : |[Relative] = None
    var underground = true
    var awake = false

    var powers : $[DragonCard] = $
    var moved : Boolean = false
    var path : $[Relative] = $

    var wakefulness = 0

    def armor = (wakefulness + 4) / 4
    def spirit = (wakefulness + 14) / 4

    var eaten = 0
    var treasures = 0
    var events = 0

    var gems : $[Power] = $

    var shriek = false

    def eat(n : Int) {
        val e = eaten

        eaten += n

        if (eaten > 7)
            eaten = 7

        if (eaten > e) {
            val d = eaten - e
            game.log(faction, "ate", d.hl, ("Goblin" + (d > 1).??("s")).styled(Goblins))
        }
    }

    val greed = new TrackValue(this, Greed, $)
    val hunger = new TrackValue(this, Hunger, $)
    val prideE = new TrackValue(this, PrideEvents, $(PridePlaceGem, PrideDontMove))
    val prideG = new TrackValue(this, PridePlaceGem, $(PrideEvents, PrideDontMove))
    val prideS = new TrackValue(this, PrideDontMove, $(PrideEvents, PridePlaceGem))

    val tracks = $(greed, hunger, prideE, prideG, prideS)

    def positions = position.$
}

trait Omen extends Record with Elementary {
    def id : String
    def img : String = "omen-" + id
    def icon : Image = Image("omen-icon-" + id, styles.token)
    def elem = toString.styled(Cave)
    val near : $[Omen]
}

case class OmenCard(id : String, omen : Omen) extends Record with Elementary {
    def img = Image(imgid, styles.power)
    def imgs = Image(imgsid, styles.power)
    def imgid = "omen-" + omen.id
    def imgsid = "omen-icon-" + omen.id
    def elem = omen.elem
}

case object Bat extends Omen { def id = "bat" ; lazy val near = $(Boulder, Quartz) }
case object Boulder extends Omen { def id = "boulder" ; lazy val near = $(Bat, Quartz, Mushroom) }
case object Quartz extends Omen { def id = "quartz" ; lazy val near = $(Bat, Boulder, Mushroom, Trail) }
case object Mushroom extends Omen { def id = "mushroom" ; lazy val near = $(Boulder, Quartz, Trail, Chasm) }
case object Trail extends Omen { def id = "trail" ; lazy val near = $(Quartz, Mushroom, Chasm) }
case object Chasm extends Omen { def id = "chasm" ; lazy val near = $(Mushroom, Trail) }

object OmenCards {
    val all : $[OmenCard] = $(Bat, Boulder, Quartz, Mushroom, Trail, Chasm)./~(o => 1.to(6)./(i => OmenCard(i.toString, o)))
}

class CavePlayer(val game : Game, val faction : Cave.type) extends Player {
    def positions = $

    var plunders = 0
    var hatreds = 0

    var omens : $[OmenCard] = $
}

sealed trait ThiefUpgrade extends Record with Elementary
case object LockPickingKit extends ThiefUpgrade { def elem = "Lock Picking Kit".hl }
case object ClimbingGear extends ThiefUpgrade { def elem = "Climbing Gear".hl }
case object HandCrossbow extends ThiefUpgrade { def elem = "Hand Crossbow".hl }
case object StickyFingers extends ThiefUpgrade { def elem = "Sticky Fingers".hl }
case object UnnaturalEvasion extends ThiefUpgrade { def elem = "Unnatural Evasion".hl }
case class StatBoost(stat : String) extends ThiefUpgrade { def elem = ("+1 " + stat).hl }

class ThiefPlayer(val game : Game, val faction : Thief.type) extends Player {
    var position = Relative(0, 0)
    var statsAssigned = false
    var movement = 0
    var stealth = 0
    var thievery = 0
    var actionCubes = 0
    var lootDrop = 3
    var carried : $[Token] = $
    var stashed = 0
    var path : $[Relative] = $
    var targeted : $[Faction] = $
    var upgrades : $[ThiefUpgrade] = $
    var usedStickyFingers = false
    var usedEvasion = false
    var dead = false

    def moves = path.num
    def effectiveStealth = max(0, stealth - carried.num)
    def positions = $(position)
}


trait TileKey extends Key { self : Action =>
    val position : Relative
}

trait ViewTile extends ViewObject[(Tile, Bearing)] { self : UserAction =>
    def tile : Tile
    def rotation : Bearing
    def obj = (tile, rotation)
}

trait ViewSecret extends ViewObject[Secret] { self : UserAction =>
    def s : Secret
    def obj = s
}

trait ViewEquipment extends ViewObject[Equipment] { self : UserAction =>
    def q : Equipment
    def obj = q
}

trait ViewSideQuest extends ViewObject[SideQuest] { self : UserAction =>
    def q : SideQuest
    def obj = q
}

trait ViewEvent extends ViewObject[Event] { self : UserAction =>
    def e : Event
    def obj = e
}

case class StartAction(version : String) extends StartGameAction with GameVersion
case class OptionsAction(setup : $[Faction], options : $[Meta.O]) extends ForcedAction
case object StartSetupAction extends ForcedAction
case class InitialTilesAction(shuffled1 : $[HiddenTile], shuffled2 : $[HiddenTile], shuffled3 : $[HiddenTile]) extends Shuffled3Action[HiddenTile, HiddenTile, HiddenTile]
case class GroupedTilesAction(shuffled1 : $[HiddenTile], shuffled2 : $[HiddenTile], shuffled3 : $[HiddenTile]) extends Shuffled3Action[HiddenTile, HiddenTile, HiddenTile]
case class AddCollapsedTilesAction(shuffled : $[HiddenTile], then : ForcedAction) extends ShuffledAction[HiddenTile]
case class ShuffledTreasuresEventsQuestsAction(f : Knight.type, shuffled1 : $[Treasure], shuffled2 : $[Event], shuffled3 : $[SideQuest]) extends Shuffled3Action[Treasure, Event, SideQuest]

case object SetupNextAction extends ForcedAction
case class SetupFactionAction(f : Faction) extends ForcedAction

case object InitDoneAction extends ForcedAction
case class StartPlayerTurnAction(f : Faction) extends ForcedAction
case class ContinuePlayerTurnAction(f : Faction) extends ForcedAction
case class KnightTurnAction(f : Knight.type) extends ForcedAction
case class ThiefTurnAction(f : Thief.type) extends ForcedAction
case class CompleteActionTurnAction(f : Faction) extends ForcedAction
case class EndPlayerTurnAction(f : Faction) extends ForcedAction


trait MoveAction { self : UserAction =>
    def direction : Bearing
}

case class CollapseTileAction(f : Faction, position : Relative, then : ForcedAction, fail : ForcedAction) extends ForcedAction with TileKey
case class ReconnectMapAction(f : Faction, mark : Boolean, then : ForcedAction) extends ForcedAction
case class MoveMapSegmentAction(self : Faction, l : $[Absolute], direction : Bearing, n : Int, then : ForcedAction) extends BaseAction("Map segment", g => l./~(p => g.board.read(self, Relative(p.x - g.board.center.x, p.y - g.board.center.y))).distinct.some./(_.join("+")).|("?").hl)("Move", direction.elem ~ (direction.dy == 0).?(" ".txt) ~ Image("move-deg-" + direction.rotation * 90, styles.token, "")) with MoveAction
case class ShowOpenEdgesAction(f : Faction, then : ForcedAction) extends ForcedAction
case class FillOpenEdgesAction(f : Faction, l : $[Relative], then : ForcedAction) extends ForcedAction
case class PlaceHiddenTileAction(f : Faction, position : Relative, t : HiddenTile, then : ForcedAction) extends ForcedAction with TileKey
case class HideOpenEdgesAction(then : ForcedAction) extends ForcedAction

trait ThiefAction { self : Action => }
case class ThiefAssignStatsAction(self : Thief.type, movement : Int, stealth : Int, thievery : Int) extends BaseAction("Assign stat tokens".styled(self))("Move", movement.hl, "Stealth", stealth.hl, "Thievery", thievery.hl) with ThiefAction
case class ThiefMoveAction(self : Thief.type, direction : Bearing, dark : Boolean) extends BaseAction("Move".styled(self))("Move", direction.elem ~ (direction.dy == 0).?(" ".txt) ~ Image("move-deg-" + direction.rotation * 90, styles.token, ""), dark.?("Dark".hl)) with MoveAction with ThiefAction
case class ThiefKeepDarkAction(self : Thief.type) extends BaseAction("Dark tile".styled(self))("Keep hidden") with ThiefAction
case class ThiefLootAction(self : Thief.type, token : Token, cubes : Int) extends BaseAction("Loot".styled(self))(cubes.hl, "cube".s(cubes).hl, dt.Arrow, token) with ThiefAction
case class ThiefLootRollAction(f : Thief.type, token : Token, random : Pattern) extends RandomAction[Pattern] with ThiefAction
case class ThiefPickLockAction(self : Thief.type, cubes : Int) extends BaseAction("Pick Lock".styled(self))(cubes.hl, "cube".s(cubes).hl, dt.Arrow, "open", Vault) with ThiefAction
case class ThiefPickLockRollAction(f : Thief.type, cubes : Int, random : Pattern) extends RandomAction[Pattern] with ThiefAction
case class ThiefPickpocketAction(self : Thief.type, target : Faction, cubes : Int) extends BaseAction("Pickpocket".styled(self))(target, cubes.hl, "cube".s(cubes).hl) with ThiefAction
case class ThiefPickpocketRollAction(f : Thief.type, target : Faction, cubes : Int, random : Pattern) extends RandomAction[Pattern] with ThiefAction
case class ThiefBackstabAction(self : Thief.type, target : AttackTarget, cubes : Int) extends BaseAction("Backstab".styled(self))(target, cubes.hl, "cube".s(cubes).hl) with ThiefAction
case class ThiefHideLootAction(self : Thief.type, cubes : Int) extends BaseAction("Hide Loot".styled(self))(cubes.hl, "cube".s(cubes).hl, dt.Arrow, "reduce Loot Drop") with ThiefAction
case class ThiefClimbAction(self : Thief.type, direction : Bearing, cubes : Int, dark : Boolean) extends BaseAction("Climb".styled(self))(cubes.hl, "cube".s(cubes).hl, dt.Arrow, "Move", direction.elem ~ (direction.dy == 0).?(" ".txt), dark.?("Dark".hl)) with MoveAction with ThiefAction
case class ThiefStashChoiceAction(self : Thief.type, upgrade : |[ThiefUpgrade]) extends BaseAction("Upgrade".styled(self))(upgrade./(u => "Place on" ~ u.elem).|("Skip")) with ThiefAction

// KNIGHT
trait KnightTurnQuestion extends FactionAction with NoClear { a : UserAction =>
    override def self : Knight.type

    def question(implicit game : Game) = self.elem ~ SpacedDash ~ knightToPlayer(self).free.times(dt.Stamina) ~ SpacedDash ~
        (self.movement - self.moves).times(dt.MovementK) ~
        (self.perception - self.encounters).times(dt.PerceptionK) ~
        (self.strength - self.strbonus).times(dt.StrengthK) ~ (self.strbonus).times(dt.Strength)
}

case class KnightAssignAction(self : Knight.type, q : Equipment, then : ForcedAction) extends ForcedAction
case class KnightRevealTreasureAction(self : Knight.type, q : Equipment) extends BaseAction()("Reveal", q)

case class KnightMoveAction(self : Knight.type, direction : Bearing, map : Boolean, encounter : Boolean) extends OptionAction("Move", direction.elem ~ (direction.dy == 0).?(" ".txt) ~ Image("move-deg-" + direction.rotation * 90, styles.token, ""), Empty ~ map.?(dt.Stamina) ~ dt.MovementK ~ encounter.?(dt.PerceptionK)) with KnightTurnQuestion with MoveAction
case class RevealTileAction(self : Faction, position : Relative, going : |[Bearing], then : ForcedAction) extends ForcedAction with TileKey
case class OrientTileAction(self : Faction, position : Relative, h : HiddenTile, rotation : Bearing, then : ForcedAction) extends BaseAction("Place tile at", position)((Image(h.tile.name, rotation).apply(styles.tile)(styles.abs) ~ h.tokens./(t => Image(t.toString.toLowerCase, $(styles.tile, styles.abs))) ~ Image("empty", $(styles.tile))).spn) with ViewTile { def tile = h.tile }

case class StartEncounterAction(self : Knight.type) extends ForcedAction
case class EncounterTileAction(self : Knight.type) extends ForcedAction
case class EncounterAttacksAction(self : Knight.type, l : $[AttackTarget], then : ForcedAction) extends ForcedAction
case class EncounterCollectAction(self : Knight.type) extends ForcedAction

case class StartEventAction(k : Knight.type) extends ForcedAction
case class ChooseEventAction(self : Cave.type, k : Knight.type, e : Event) extends BaseAction(self, "chooses event for", k)(e.img) with ViewEvent
case class ExecuteEventAction(k : Knight.type, e : Event) extends ForcedAction

case class AttackAction(self : Knight.type, e : AttackTarget, q : |[Equipment], roll : Boolean, then : ForcedAction) extends OptionAction("Attack".styled(styles.hit), e, q./(("with", _)), roll.?(dt.DragonDie)) with KnightTurnQuestion
case class AttackDamageAction(self : Knight.type, e : AttackTarget, q : |[Equipment], then : ForcedAction) extends ForcedAction
case class AttackRollAction(self : Knight.type, e : AttackTarget, q : |[Equipment], random : Pattern, then : ForcedAction) extends RandomAction[Pattern]
case class MightyAxeAction(self : Knight.type, e : AttackTarget, q : |[Equipment], then : ForcedAction) extends BaseAction(MightyAxe)("Use", MightyAxe, dt.Discard, dt.Arrow, "+1".hl, "damage")


case class CollectChestAction(self : Knight.type) extends BaseAction(Image("chest", styles.illustration))("Take", Chest)
case class DrawTreasureAction(self : Knight.type) extends ForcedAction
case class SelectTreasureAction(self : Cave.type, k : Knight.type, t : Treasure) extends BaseAction(self, "chooses treasure for", k)(Image(t.id, styles.card, styles.inline)) with ViewEquipment { def q = t.as[EquipmentTreasure].get.equipment }
case class EvaluateTreasureAction(self : Knight.type, t : Treasure) extends ForcedAction
case class TakeTreasureAction(self : Knight.type, t : Treasure) extends BaseAction(Image(t.id, styles.card, styles.inline))("Take".hl, t)
case class DeclineTreasureAction(self : Knight.type) extends BaseAction()("Decline for", 5.hl, "Grit".styled(self))

case class CollectDragonGemAction(self : Knight.type, d : DragonGem) extends BaseAction(Image("gem-" + d.power.id, styles.illustration))("Collect", "Dragon Gem".styled(d.faction))
case class CollectRollAction(self : Knight.type, random : Pattern) extends RandomAction[Pattern]

case class KnightBombMainAction(self : Knight.type) extends BaseAction()("Use", "Bomb".styled(self), dt.Stamina, dt.Arrow, "remove a wall") with Soft
case class KnightBombAction(self : Knight.type, direction : Bearing) extends BaseAction("Bomb a wall")("Bomb", direction, Image("move-deg-" + direction.rotation * 90, styles.token, "")) with MoveAction

case class KnightBowAction(self : Knight.type, e : Goblins.type, t : Tribe, n : Int) extends BaseAction()("Shoot", t, "with", Bow, dt.Stamina, dt.Arrow, n.times(dt.PopulationLost).merge)

case class KnightEnchantedBowAction(self : Knight.type, e : AttackTarget, n : Int) extends BaseAction()("Shoot", e, "with", EnchantedBow, dt.Stamina, dt.Arrow, n.times(dt.PowerAny).merge)

case class DragonDiscardPowersMainAction(self : Dragon.type, n : Int, then : ForcedAction) extends ForcedAction with Soft
case class DragonDiscardPowersAction(self : Dragon.type, l : $[PowerCard], then : ForcedAction) extends ForcedAction

case class CaveDiscardOmensMainAction(self : Cave.type, n : Int, then : ForcedAction) extends ForcedAction with Soft
case class CaveDiscardOmensAction(self : Cave.type, l : $[OmenCard], then : ForcedAction) extends ForcedAction

case class KnightShieldAction(self : Knight.type, then : ForcedAction) extends BaseAction()("Wield", Shield, dt.Stamina, dt.Arrow, "Protection".hh) with NoClear

case class KnightPotionKitAction(self : Knight.type) extends BaseAction()("Use", PotionKit, dt.Discard, dt.Arrow, "+2".hl, "Health".styled(self))

case class RecoverFromPoisionAction(self : Knight.type) extends BaseAction()("Recover from", Poison)

case class ClaimQuestAction(self : Knight.type, q : SideQuest, then : ForcedAction) extends BaseAction("Side Quests".styled(self))("Claim", q.name.styled(self)) with NoClear
case class ClaimQuestsAction(self : Knight.type, l : $[SideQuest], then : ForcedAction) extends ForcedAction

case class ForceMoveStraightMainAction(f : Faction, t : AttackTarget, n : Int, then : ForcedAction) extends ForcedAction

case class ForceMoveStraightAction(self : Faction, t : AttackTarget, direction : Bearing, n : Int, then : ForcedAction) extends BaseAction("Move", t)("Move", direction, (n > 1).?(n.hl ~ " tiles"), max(1, n).times(Image("move-deg-" + direction.rotation * 90, styles.token, ""))) with MoveAction
case class ForceMoveStraightStepAction(f : Faction, t : AttackTarget, direction : Bearing, n : Int, then : ForcedAction) extends ForcedAction

case class ForceMoveMainAction(f : Faction, t : AttackTarget, n : Int, then : ForcedAction) extends ForcedAction
case class ForceMoveAction(self : Faction, t : AttackTarget, k : Int, n : Int, path : $[Relative], then : ForcedAction) extends ForcedAction
case class ForceMoveStepAction(self : Faction, t : AttackTarget, direction : Bearing, k : Int, n : Int, path : $[Relative], then : ForcedAction) extends BaseAction("Move", t, (n - k + 1).hl, "tile".s(n - k + 1).hl)("Move", direction, Image("move-deg-" + direction.rotation * 90, styles.token, "")) with MoveAction

case class FreshHealAction(f : Knight.type, then : ForcedAction) extends ForcedAction
case class LightHealAction(f : Knight.type, then : ForcedAction) extends ForcedAction
case class CaveBreadAction(f : Knight.type, then : ForcedAction) extends ForcedAction
case class DeepAndDarkCaveAction(f : Cave.type, then : ForcedAction) extends ForcedAction
case class RatsAction(f : Knight.type, then : ForcedAction) extends ForcedAction
case class VantagePointAction(f : Knight.type, l : $[Relative], then : ForcedAction) extends ForcedAction

case class TreasureInfoAction(self : Knight.type, q : Equipment) extends BaseInfo(Break ~ "Equipment")(Image(q.id, styles.card, styles.inline)) with ViewEquipment
case class SideQuestInfoAction(self : Knight.type, q : SideQuest) extends BaseInfo(Break ~ "Side Quests")(Image("sidequest-" + q.id, styles.card, styles.inline)) with ViewSideQuest


// GOBLINS
case class WarCardsShuffledAction(f : Goblins.type, shuffled : $[WarCard]) extends ShuffledAction[WarCard]
case class WarCardsListAction(f : Goblins.type, l : $[WarCard]) extends ForcedAction
case class RedrawWarCardAction(f : Goblins.type, l : $[WarCard], c : WarCard) extends ForcedAction
case class WarCardAction(self : Goblins.type, c : WarCard, l : $[Tribe], s : |[Tribe]) extends ForcedAction

case class DrawMonstersAction(self : Goblins.type, n : Int, then : ForcedAction) extends ForcedAction
case class TakeMonstersAction(self : Goblins.type, n : Int, shuffled : $[Monster], then : ForcedAction) extends ShuffledAction[Monster]
case class AssignMonstersAction(self : Goblins.type, l : $[Monster], then : ForcedAction) extends ForcedAction

case class AddMonsterAction(self : Goblins.type, t : Tribe, m : Monster, then : ForcedAction) extends ForcedAction
case class ReplaceMonsterAction(self : Goblins.type, t : Tribe, o : Monster, m : Monster, then : ForcedAction) extends ForcedAction
case class RemoveMonsterAction(self : Goblins.type, t : Tribe, o : Monster, then : ForcedAction) extends BaseAction(t, "remove", "Monster".hh)(o)

case class DrawSecretsAction(self : Goblins.type, n : Int, then : ForcedAction) extends ForcedAction
case class TakeSecretsAction(self : Goblins.type, n : Int, shuffled : $[Secret], then : ForcedAction) extends ShuffledAction[Secret]
case class HandLimitAction(self : Goblins.type, drawn : Int, n : Int, then : ForcedAction) extends ForcedAction
case class HandLimitDiscardAction(self : Goblins.type, l : $[Secret], then : ForcedAction) extends ForcedAction
case class SecretInfoAction(self : Goblins.type, s : Secret) extends BaseInfo(Break ~ "Secrets".hl)(s.img) with ViewSecret
case class SecretDrawnInfoAction(self : Goblins.type, s : Secret) extends BaseInfo("Secrets".hl, "drawn")(s.img) with ViewSecret
case object NoHand extends HiddenInfo
case class HighlightChestAt(position : Relative) extends HiddenInfo

case class UseSecretAction(self : Goblins.type, t : |[Tribe], s : Secret, then : ForcedAction) extends ForcedAction
case class RevealSecretAction(self : Goblins.type, s : Secret, then : ForcedAction) extends ForcedAction

case class HexAction(self : Goblins.type, e : Faction, then : ForcedAction) extends ForcedAction

case class CaveInMainAction(self : Goblins.type, then : ForcedAction) extends ForcedAction
case class CaveInListAction(self : Goblins.type, l : $[Relative], t : $[Relative], then : ForcedAction) extends ForcedAction // with TileKey { val position = t.lastOption | null }
case class CaveInSelectAction(self : Goblins.type, l : $[Relative], t : $[Relative], position : Relative, then : ForcedAction) extends BaseAction("Cave-In")("Tile", g => g.board.read(self, position).|("?").hl) with Soft with NoClear with TileKey
case class CaveInCancelAction(self : Goblins.type, then : ForcedAction) extends ForcedAction
case class CaveInAction(self : Goblins.type, t : $[Relative], then : ForcedAction) extends ForcedAction

case class WarCardSecretsAction(self : Goblins.type, c : WarCard) extends ForcedAction

case class GoblinsTopAction(self : Goblins.type) extends ForcedAction
case class ActivateTribeAction(self : Goblins.type, t : Tribe) extends BaseAction(self, "activate a Tribe")("Activate".hh, t) with Soft
case class TribeTurnAction(self : Goblins.type, t : Tribe) extends ForcedAction with Soft
case class UnlurkTribeAction(self : Goblins.type, t : Tribe) extends BaseAction(self, "activate a Tribe")("Reveal".hh, "or", "Hide".hh, t) with Soft
case class RevealTribeAction(self : Goblins.type, t : Tribe, cancel : ForcedAction) extends ForcedAction
case class RevealTribeCellAction(self : Goblins.type, t : Tribe, position : Relative) extends ForcedAction with TileKey
case class RevealTribeCancelAction(self : Goblins.type, t : Tribe, then : ForcedAction) extends ForcedAction
case class DoneTribeAction(self : Goblins.type, t : Tribe) extends ForcedAction
case class HideTribeAction(self : Goblins.type, t : Tribe) extends ForcedAction
case class IncreaseRageAction(self : Goblins.type, t : Tribe) extends ForcedAction
case class DrawMonsterAction(self : Goblins.type, t : Tribe) extends ForcedAction
case class DrawSecretAction(self : Goblins.type, t : Tribe) extends ForcedAction

case class ReducePopulationAction(self : Goblins.type, t : Tribe, n : Int, then : ForcedAction) extends ForcedAction
case class ScatterTribeAction(self : Goblins.type, t : Tribe, then : ForcedAction) extends ForcedAction
case class ProcessAmbushAction(f : Knight.type, then : ForcedAction) extends ForcedAction
case class AttackAmbushAction(g : Goblins.type, k : Knight.type, then : ForcedAction) extends ForcedAction
case class SkipAmbushAction(g : Goblins.type, k : Knight.type, then : ForcedAction) extends ForcedAction
case class RemoveAmbushAction(f : Knight.type, then : ForcedAction) extends ForcedAction

trait GoblinsTurnQuestion extends FactionAction {
    def tribe : Tribe
    override def self : Goblins.type

    def question(implicit game : Game) = tribe.elem ~ SpacedDash ~ "Turn"
}

case class GoblinsMoveScatterAction(self : Goblins.type, tribe : Tribe, direction : Bearing,                   then : ForcedAction) extends OptionAction("Scatter"                   , direction, Image("move-deg-" + direction.rotation * 90, styles.token, "")) with GoblinsTurnQuestion with MoveAction
case class GoblinsMoveAction       (self : Goblins.type, tribe : Tribe, direction : Bearing, reduce : Boolean, then : ForcedAction) extends OptionAction("Move"                      , direction, Image("move-deg-" + direction.rotation * 90, styles.token, "")) with GoblinsTurnQuestion with MoveAction with NoClear
case class GoblinsMoveExploreAction(self : Goblins.type, tribe : Tribe, direction : Bearing, reduce : Boolean, then : ForcedAction) extends OptionAction("Explore".styled(self)      , direction, Image("move-deg-" + direction.rotation * 90, styles.token, "")) with GoblinsTurnQuestion with MoveAction
case class GoblinsMoveAttackAction (self : Goblins.type, tribe : Tribe, direction : Bearing, reduce : Boolean, then : ForcedAction) extends OptionAction("Attack" .styled(styles.hit), direction, Image("move-deg-" + direction.rotation * 90, styles.token, "")) with GoblinsTurnQuestion with MoveAction

case class GoblinsMoveLightAction(self : Goblins.type, tribe : Tribe, reduce : Boolean, then : ForcedAction) extends ForcedAction

case class GoblinsExploreAction(self : Goblins.type, tribe : Tribe, then : ForcedAction) extends ForcedAction
case class GoblinsAttackAction(self : Goblins.type, tribe : Tribe, e : Faction, then : ForcedAction) extends ForcedAction
case class HalfSmashCrystalAction(self : Goblins.type, tribe : Tribe) extends ForcedAction
case class PlunderChestAction(self : Goblins.type, tribe : Tribe) extends ForcedAction
case class PlunderDragonGemAction(self : Goblins.type, tribe : Tribe, d : DragonGem) extends ForcedAction
case class PlunderDragonAction(self : Goblins.type, tribe : Tribe, d : Dragon.type) extends ForcedAction
case class PlunderRollAction(self : Goblins.type, tribe : Tribe, random : Pattern) extends RandomAction[Pattern]

case class UnderwormMainAction(self : Goblins.type, tribe : Tribe, reduce : Boolean) extends ForcedAction
case class UnderwormAction(self : Goblins.type, tribe : Tribe, reduce : Boolean, position : Relative) extends ForcedAction with TileKey
case class UnderwormCancelAction(self : Goblins.type, tribe : Tribe) extends ForcedAction

case class WispMainAction(self : Goblins.type, tribe : Tribe, e : Knight.type) extends ForcedAction

case class AddEffectAction(self : Goblins.type, tribe : Tribe, s : Effect, then : ForcedAction) extends ForcedAction
case class UseTrapAction(self : Goblins.type, then : ForcedAction) extends ForcedAction
case class IgnoreTrapAction(self : Goblins.type, then : ForcedAction) extends ForcedAction
case class ApplyPoisonAction(self : Goblins.type, t : Knight.type, n : Int, then : ForcedAction) extends ForcedAction


// DRAGON
case class DragonCheckAction(f : Dragon.type, then : ForcedAction) extends ForcedAction

case class DragonTurnAction(f : Dragon.type) extends ForcedAction
case class DragonMainAction(f : Dragon.type) extends ForcedAction
case class DragonMoveMainAction(f : Dragon.type, wall : Boolean, l : $[DragonCard]) extends ForcedAction with Soft
case class DragonMoveAction(self : Dragon.type, direction : Bearing, l : $[DragonCard], then : ForcedAction) extends BaseAction(Dragon, "moves")("Move", direction.elem ~ (direction.dy == 0).?(" ".txt) ~ Image("move-deg-" + direction.rotation * 90, styles.token, "")) with MoveAction
case class DragonMoveContinueAction(f : Dragon.type, direction : Bearing) extends ForcedAction with Soft
case class PromptAttacksAction(f : Dragon.type, then : ForcedAction) extends ForcedAction

case class DragonRevealMainAction(f : Dragon.type, l : $[DragonCard]) extends ForcedAction
case class DragonRevealRollAction(f : Dragon.type, shriek : Boolean) extends ForcedAction
case class DragonRevealRolledAction(f : Dragon.type, random : Pattern) extends RandomAction[Pattern]
case class DragonRevealAction(f : Dragon.type, p : Pattern) extends ForcedAction
case class DragonRevealListAction(f : Dragon.type, l : $[Relative], x : Pattern) extends ForcedAction


case class DragonScorchMainAction(f : Dragon.type, l : $[DragonCard]) extends ForcedAction

case class DragonSlitherMainAction(f : Dragon.type, l : $[DragonCard]) extends ForcedAction with Soft
case class DragonSlitherFromAction(f : Dragon.type, l : $[DragonCard], t : Track) extends ForcedAction with Soft
case class DragonSlitherToAction(f : Dragon.type, l : $[DragonCard], t : Track, d : Track) extends ForcedAction

case class DragonBurnMainAction(f : Dragon.type, l : $[DragonCard]) extends ForcedAction
case class DragonBurnAction(f : Dragon.type, l : $[DragonCard], position : Relative) extends ForcedAction with TileKey


case class DragonAttackMainAction(f : Dragon.type, l : $[DragonCard]) extends ForcedAction
case class DragonAttackRollAction(f : Dragon.type, shriek : Boolean) extends ForcedAction
case class DragonAttackRolledAction(f : Dragon.type, random : Pattern) extends RandomAction[Pattern]
case class DragonAttackAction(f : Dragon.type, p : Pattern) extends ForcedAction
case class DragonAttackListAction(f : Dragon.type, x : Pattern, l : $[AttackTarget]) extends ForcedAction
case class DragonAttackTargetAction(f : Dragon.type, t : AttackTarget, then : ForcedAction) extends ForcedAction


case class DrawPowersAction(f : Dragon.type, n : Int, then : ForcedAction) extends ForcedAction
case class PowersShuffledAction(f : Dragon.type, n : Int, shuffled : $[DragonCard], then : ForcedAction) extends ShuffledAction[DragonCard]

case class DragonHissMainAction(f : Dragon.type, l : $[DragonCard]) extends ForcedAction with Soft
case class DragonHissAction(self : Dragon.type, e : Goblins.type, t : Tribe, l : $[DragonCard]) extends BaseAction(self, "hisses at")(t)
case class DragonHissIgnoreAction(self : Dragon.type, e : Goblins.type, t : Tribe, l : $[DragonCard], then : ForcedAction) extends ForcedAction
case class DragonHissEatAction(f : Dragon.type, e : Goblins.type, t : Tribe, old : Int) extends ForcedAction

case class DragonShriekMainAction(f : Dragon.type, l : $[DragonCard]) extends ForcedAction

case class DragonSmashMainAction(f : Dragon.type, l : $[DragonCard]) extends ForcedAction

case class DragonWrathMainAction(f : Dragon.type, l : $[DragonCard]) extends ForcedAction
case class DragonWrathRollAction(f : Dragon.type, shriek : Boolean) extends ForcedAction
case class DragonWrathRolledAction(f : Dragon.type, random : Pattern) extends RandomAction[Pattern]
case class DragonWrathAction(f : Dragon.type, p : Pattern) extends ForcedAction

case class DragonScratchMainAction(f : Dragon.type, l : $[DragonCard]) extends ForcedAction with Soft
case class DragonScratchAction(f : Dragon.type, l : $[DragonCard], e : Faction) extends ForcedAction
case class DragonScratchEatAction(f : Dragon.type, e : Goblins.type) extends ForcedAction

case class DragonSlapMainAction(f : Dragon.type, l : $[DragonCard]) extends ForcedAction with Soft
case class DragonSlapAction(f : Dragon.type, l : $[DragonCard], k : Knight.type) extends ForcedAction

case class DragonSwatMainAction(f : Dragon.type, l : $[DragonCard]) extends ForcedAction with Soft
case class DragonSwatAction(f : Dragon.type, l : $[DragonCard], t : TokenAt) extends ForcedAction

case class DragonFlameWallMainAction(f : Dragon.type, l : $[DragonCard]) extends ForcedAction with Soft
case class DragonFlameWallAction(self : Dragon.type, l : $[DragonCard], direction : Bearing) extends BaseAction("Place", "Flame Wall".styled(self))("Flame", direction, Image("move-deg-" + direction.rotation * 90, styles.token, "")) with MoveAction

case class DragonDoneForfeitAction(f : Dragon.type) extends ForcedAction with Soft
case class DragonEndAction(f : Dragon.type) extends ForcedAction
case class DragonTreasuresAction(f : Dragon.type) extends ForcedAction
case class DragonPickTreasureAction(self : Dragon.type, t : Chest.type) extends BaseAction(Image("chest", styles.illustration))("Take", Chest)
case class DragonPlaceGemsAction(f : Dragon.type) extends ForcedAction
case class DragonPlaceGemAction(f : Dragon.type, p : Power) extends ForcedAction
case class DragonRedrawAction(f : Dragon.type) extends ForcedAction

case class PlaceGemAction(f : Dragon.type, p : Power) extends ForcedAction

case class PowerInfoAction(self : Dragon.type, p : DragonCard) extends BaseInfo(Break ~ "Powers")(p.img) with ViewObject[DragonCard] { def obj = p }
case object NoPower extends HiddenInfo


// CAVE
case class DrawOmensAction(f : Cave.type, n : Int, then : ForcedAction) extends ForcedAction
case class OmensShuffledAction(f : Cave.type, n : Int, shuffled : $[OmenCard], then : ForcedAction) extends ShuffledAction[OmenCard]
case class CaveMainAction(f : Cave.type) extends ForcedAction
case class CaveDoneAction(f : Cave.type) extends ForcedAction
case class PlaceTileAction(f : Cave.type, then : ForcedAction) extends ForcedAction
case class RemoveTileAction(f : Cave.type, l : $[Relative], then : ForcedAction) extends ForcedAction
case class PlaceTreasureAction(f : Cave.type, then : ForcedAction) extends ForcedAction
case class PlaceTreasureTileAction(self : Cave.type, position : Relative, then : ForcedAction) extends BaseAction(Image("chest", styles.illustration))("Place", Chest, "at", g => g.board.read(self, position).|("?").hl) with TileKey

case class CaveCancelAction(self : Cave.type) extends ForcedAction

case class GiantBatsKnightAction(self : Cave.type, e : Knight.type, l : $[OmenCard]) extends ForcedAction
case class GiantBatsGoblinsAction(self : Cave.type, e : Goblins.type, t : Tribe, l : $[OmenCard]) extends ForcedAction
case class GiantBatsChestMainAction(self : Cave.type, l : $[OmenCard]) extends ForcedAction
case class GiantBatsChestAction(self : Cave.type, e : TokenAt, l : $[OmenCard]) extends ForcedAction

case class RockslideMainAction(self : Cave.type, l : $[OmenCard]) extends ForcedAction
case class RockslideRemoveAction(self : Cave.type, p : Relative, direction : Bearing, then : ForcedAction) extends ForcedAction
case class RockslideAction(self : Cave.type, p : Relative, direction : Bearing, l : $[OmenCard]) extends ForcedAction

case class PlaceTreasureMainAction(self : Cave.type, l : $[OmenCard]) extends ForcedAction

case class SoporificSporesMainAction(self : Cave.type, e : Faction, l : $[OmenCard]) extends ForcedAction
case class ReturnSlothAction(f : Dragon.type, t : Track, then : ForcedAction) extends ForcedAction

case class PlaceTileMainAction(self : Cave.type, l : $[OmenCard]) extends ForcedAction
case class RemoveTileMainAction(self : Cave.type, l : $[OmenCard]) extends ForcedAction

case class CrystalCurseRotateTileMainAction(self : Cave.type, l : $[OmenCard]) extends ForcedAction
case class RotateTileMainAction(self : Cave.type, position : Relative, l : $[OmenCard]) extends ForcedAction with TileKey
case class RotateTileAction(self : Cave.type, tile : Tile, p : Relative, rotation : Bearing, l : $[OmenCard]) extends BaseAction("Rotate tile", _.board.read(self, p).|("?").hl)((Image(tile.name, rotation).apply(styles.tile)).spn) with ViewTile

case class CrystalCurseEventTokenMainAction(self : Cave.type, l : $[OmenCard]) extends ForcedAction
case class EventTokenAction(self : Cave.type, position : Relative, l : $[OmenCard]) extends ForcedAction with TileKey

case class CrystalCurseRecycleEventsMainAction(self : Cave.type, l : $[OmenCard]) extends ForcedAction

case class EventInfoAction(self : Cave.type, e : Event) extends BaseInfo(Break ~ "Events")(e.img) with ViewEvent
case class OmenInfoAction(self : Cave.type, o : OmenCard) extends BaseInfo(Break ~ "Omens")(o.img) with ViewObject[OmenCard] { def obj = o }
case object NoOmen extends HiddenInfo

case class GameOverAction(winners : $[Faction]) extends ForcedAction
case class GameOverWonAction(self : Faction, f : Faction) extends BaseInfo("Game Over" ~ f.@@{
    case Knight => Image("knight", styles.winnerKnight)
    case Dragon => Image("dragon-awake", styles.winnerDragon)
    case Goblins => Image("tribe-fangs", styles.winnerGoblins) ~ Image("tribe-bones", styles.winnerGoblins) ~ Image("tribe-eye", styles.winnerGoblins)
    case Cave => Image("cave", styles.winnerCave)
}.div)(f, "won", "(" ~ NameReference(f.name, f).hl ~ ")")


case class PlaceMagmaAction(f : Faction, p : Relative, then : ForcedAction) extends ForcedAction
case class PlaceRiverAction(f : Faction, p : Relative, then : ForcedAction) extends ForcedAction
case class PlaceNothingAction(f : Faction, then : ForcedAction) extends ForcedAction


trait GameImplicits {
    implicit def knightToPlayer(f : Knight.type)(implicit game : Game) = game.states(f).asInstanceOf[KnightPlayer]
    implicit def goblinsToPlayer(f : Goblins.type)(implicit game : Game) = game.states(f).asInstanceOf[GoblinsPlayer]
    implicit def dragonToPlayer(f : Dragon.type)(implicit game : Game) = game.states(f).asInstanceOf[DragonPlayer]
    implicit def caveToPlayer(f : Cave.type)(implicit game : Game) = game.states(f).asInstanceOf[CavePlayer]
    implicit def thiefToPlayer(f : Thief.type)(implicit game : Game) = game.states(f).asInstanceOf[ThiefPlayer]
    implicit def factionToPlayer(f : Faction)(implicit game : Game) = game.states(f)

    implicit class FactionLogScore(f : Faction)(implicit game : Game) {
        def log(s : Any*) {
            if (game.logging)
                game.log((f +: s.$) : _*)
        }
    }
}

class Game(val setup : $[Faction], val options : $[Meta.O]) extends BaseGame with ContinueGame with LoggedGame with GameThiefSupport with GameSetupSupport with GameGoblinsSupport with GameDragonSupport with GameCaveTailSupport with GameBoardTailSupport {
    private implicit val game = this

    private def logMarker(scope : String, block : String, message : String) : Unit = {
        +++("[VastGame][" + scope + "][" + block + "] " + message)
    }

    var isOver = false

    var factions : $[Faction] = $

    var states = Map[Faction, Player]()

    var turn : Int = 0

    val board : Board = new Board

    var tiles : $[HiddenTile] = $
    var collapsed : $[HiddenTile] = $
    var treasures : $[Treasure] = $
    var events : $[Event] = $

    var revealed : Int = 0
    var smashed : Int = 0

    var collapse : Boolean = false
    var shrunk : Int = 0

    var current : Faction = Knight

    var highlightFaction : $[Faction] = $

    def info(waiting : $[Faction], self : |[Faction], actions : $[UserAction]) : $[Info] = {
        self.%(states.contains)./~{
            case f : Knight.type =>
                f.invertory./(q => TreasureInfoAction(f, q)) ++
                f.quests./(q => SideQuestInfoAction(f, q))

            case f : Goblins.type =>
                actions.has(NoHand).not.??(f.hand./(SecretInfoAction(f, _)))

            case f : Dragon.type =>
                actions.has(NoPower).not.??(f.powers./(p => PowerInfoAction(f, p)))

            case f : Cave.type =>
                actions.has(NoOmen).not.??(f.omens./(o => OmenInfoAction(f, o))) ++
                events.take(3)./(e => EventInfoAction(f, e))

            case _ => $()
        }
    }

    def convertForLog(s : $[Any]) : $[Any] = s./~{
        case Empty => None
        case NotInLog(_) => None
        case AltInLog(_, m) => Some(m)
        case f : Faction => Some(f.elem)
        case q : Equipment if q.is[Stat].not => Some(OnClick(q, q.elemLog.spn(xlo.pointer)))
        case e : Event => Some(OnClick(e, e.elemLog.spn(xlo.pointer)))
        case s : SideQuest => Some(OnClick(s, s.elemLog.spn(xlo.pointer)))
        case d : WarCard => Some(OnClick(d, d.elemLog.spn(xlo.pointer)))
        case m : Monster => Some(OnClick(m, m.elemLog.spn(xlo.pointer)))
        case s : Secret => Some(OnClick(s, s.elemLog.spn(xlo.pointer)))
        case l : $[Any] => convertForLog(l)
        case x => Some(x)
    }

    implicit def descSecret(g : Game, s : Secret) = s.img

    override def log(s : Any*) {
        super.log(convertForLog(s.$) : _*)
    }

    // START_CONTRACT: loggedPerform
    // PURPOSE: Execute a game action, validate resulting positions, and refresh highlight state for the next Continue.
    // INPUTS: { action: Action - action to resolve, soft: Void - soft execution flag passed through to performInternal }
    // OUTPUTS: { Continue - next continuation for the game loop }
    // SIDE_EFFECTS: mutates game state, emits player-facing logs, emits stable verification markers
    // LINKS: M-VAST-GAME, V-M-VAST-GAME
    // END_CONTRACT: loggedPerform
    def loggedPerform(action : Action, soft : Void) : Continue = {
        // START_BLOCK_PERFORM_ACTION
        logMarker("performAction", "BLOCK_PERFORM_ACTION", "action=" + action.getClass.getSimpleName + " turn=" + turn)
        val c = performInternal(action, soft)
        // END_BLOCK_PERFORM_ACTION

        // START_BLOCK_VALIDATE_POSITIONS
        factions.foreach { f =>
            if (states.contains(f)) {
                f.positions.foreach { p =>
                    if (board.valid(p).not) {
                        error("[VastGame][validateState][BLOCK_VALIDATE_POSITIONS] faction=" + f + " position=" + p)
                        println("out of map " + f + " " + p)
                        throw new Error("out of map " + f + " " + p)
                    }
                }
            }
        }
        // END_BLOCK_VALIDATE_POSITIONS

        // START_BLOCK_SET_HIGHLIGHT
        highlightFaction = c match {
            case Ask(f, _) => $(f)
            case MultiAsk(a, _) => a./(_.faction)
            case _ => Nil
        }
        // END_BLOCK_SET_HIGHLIGHT

        c
    }

    def assignStamina(f : Knight.type, then : ForcedAction, move : Boolean, perc : Boolean, str : Boolean)(implicit game : Game, ask : ActionCollector) {
        if (f.free <= 0)
            return

        if (move)
            + KnightAssignAction(f, Movement, then).as("Increase", Movement, f.assigned.count(Movement).times(dt.Used) ~ dt.Stamina, dt.Arrow, "+2".hl, "Moves".styled(f), dt.MovementK ~ dt.MovementK).noClear
                .!(f.assigned.count(Movement) >= 3, "max")
                .!(f.perception <= f.encounters, "no encounters")

        if (perc)
            + KnightAssignAction(f, Perception, then).as("Increase", Perception, f.assigned.count(Perception).times(dt.Used) ~ dt.Stamina, dt.Arrow, "+1".hl, "Encounter".styled(f), dt.PerceptionK).noClear
                .!(f.assigned.count(Perception) >= 3, "max")

        if (str)
            + KnightAssignAction(f, Strength, then).as("Increase", Strength, f.assigned.count(Strength).times(dt.Used) ~ dt.Stamina, dt.Arrow, "+1".hl, "Strength".styled(f), dt.StrengthK).noClear
                .!(f.assigned.count(Strength) >= 3, "max")

        if (move && f.invertory.has(HeroicBoots))
            + KnightAssignAction(f, HeroicBoots, then).as("Use", HeroicBoots, dt.Stamina, dt.Arrow, "+4".hl, Movement, dt.MovementK ~ dt.MovementK ~ dt.MovementK ~ dt.MovementK).noClear
                .!(f.assigned.has(HeroicBoots), "")
                .!(f.perception <= f.encounters, "no encounters")

        if ((move || perc) && f.invertory.has(PixieLantern))
            + KnightAssignAction(f, PixieLantern, then).as("Use", PixieLantern, dt.Stamina, dt.Arrow, "+1".hl, Movement, dt.MovementK, "and", "+1".hl, Perception, dt.PerceptionK).noClear
                .!(f.assigned.has(PixieLantern), "")

        if ((perc || str) && f.invertory.has(ElvishSword))
            + KnightAssignAction(f, ElvishSword, then).as("Use", ElvishSword, dt.Stamina, dt.Arrow, "+1".hl, Perception, dt.PerceptionK, "and", "+1".hl, Strength, dt.Strength, "during your turn").noClear
                .!(f.assigned.has(ElvishSword), "")

        if (str && f.invertory.has(MightyAxe))
            + KnightAssignAction(f, MightyAxe, then).as("Use", MightyAxe, dt.Stamina, dt.Arrow, "+1".hl, Strength, dt.Strength, "during your turn").noClear
                .!(f.assigned.has(MightyAxe), "")
    }

    def performInternal(a : Action, soft : Void) : Continue = {
        implicit val action = a

        action match {
            // INIT
            case StartAction(version) =>
                log("HRF".hl, "version", gaming.version.hl)
                log("Vast: The Crystal Caverns".hl)

                if (version != gaming.version)
                    log("Saved game version", version.hl)

                Milestone(OptionsAction(setup, options))

            case OptionsAction(setup, options) =>
                options.foreach { o =>
                    log(o.group, o.valueOn)
                }

                StartSetupAction

            case StartSetupAction =>
                startSetup()

            case InitialTilesAction(m, c, v) =>
                initialTiles(m, c, v)

            case GroupedTilesAction(l1, l2, l3) =>
                groupedTiles(l1, l2, l3)

            case SetupNextAction =>
                setupNext()

            case SetupFactionAction(f) =>
                setupFaction(f)

            case ShuffledTreasuresEventsQuestsAction(f, t, e, q) =>
                setupKnightDraws(f, t, e, q)

            case InitDoneAction =>
                initDone()

            case StartPlayerTurnAction(f) =>
                log(DoubleLine)

                turn += 1
                current = f

                log("Turn", ("#" + turn).hl, "-", f)

                factions = factions.dropWhile(_ != f) ++ factions.takeWhile(_ != f)

                if (collapse.not && tiles.none) {
                    collapse = true

                    log("The", "Collapse".styled(Cave), "has started...")
                }

                Milestone(ContinuePlayerTurnAction(f))

            // KNIGHT
            case ContinuePlayerTurnAction(f : Knight.type) =>
                f.assigned = $
                f.path = $
                f.encountered = $
                f.attacked = $

                f.log("regained", Stamina)

                KnightTurnAction(f)

            case KnightTurnAction(f) =>
                implicit val ask = builder

                val origin = f.position

                Bearings.wnes.foreach { dir =>
                    val map = board.wall(origin, dir)

                    val dest = origin.add(dir)

                    val cell = board.get(dest)

                    var targets = factions.of[Dragon.type].%(_.underground.not).%(_.position.has(dest))./(_.armor) ++ factions.of[Goblins.type]./~(e => e.tribes.%(_.position.has(dest))./(_.strength + 1 + e.revealed.has(Trap).??(1)))

                    if (targets.any && f.invertory.has(Javelin)) {
                        val m = targets.max

                        targets :-= m

                        targets :+= m - 1
                    }

                    val encounter = targets.any || cell.is[HiddenTile] || cell.is[Emptiness.type] || board.list(dest).has(Event)

                    + KnightMoveAction(f, dir, map, encounter)
                        .!(board.list(origin).has(Event))
                        .!(f.moves >= f.movement, "no moves")
                        .!(f.encounters >= f.perception, "no encounters")
                        .!(cell.is[Emptiness.type] && map, "open space")
                        .!(cell.is[Emptiness.type] && tiles.none, "no more tiles")
                        .!(map && f.free <= 0, "ancient map required")
                        .!(map && f.assigned.count(AncientMap) >= 3, "ancient map required")
                        .!(targets.exists(_ > f.strength), "not enough strength")
                }

                if (f.poison > 0 && f.position == board.entrance)
                    + RecoverFromPoisionAction(f)

                if (revealed >= 10 && f.quests.has(Intrepid))
                    + ClaimQuestAction(f, Intrepid, KnightTurnAction(f))

                if (smashed >= 3 && f.quests.has(Persistent))
                    + ClaimQuestAction(f, Persistent, KnightTurnAction(f))

                if (f.arsenal.num >= 2 && f.quests.has(Bedecked))
                    + ClaimQuestAction(f, Bedecked, KnightTurnAction(f))

                if (f.path.distinct.num >= 7 && f.quests.has(Swift))
                    + ClaimQuestAction(f, Swift, KnightTurnAction(f))

                if (f.encountered.distinct.num >= 3 && f.quests.has(Adventurous))
                    + ClaimQuestAction(f, Adventurous, KnightTurnAction(f))

                if (f.encounters < f.perception) {
                    var r : $[Elem] = $

                    def maxStr(s : Int) = f.strength + f.invertory.count(Javelin) + min(s, (3 - f.assigned.count(Strength)) + (f.invertory.count(ElvishSword) - f.assigned.count(ElvishSword)) + (f.invertory.count(MightyAxe) - f.assigned.count(MightyAxe)))

                    if (board.list(origin).has(Event)) {
                        r :+= "Handle " ~ Event.elem
                    }

                    if (factions.of[Dragon.type]
                        .%(_.position.has(origin))
                        .%(_.armor <= maxStr(f.free))
                        .%(e => e.underground.not || f.attacked.has(e).not)
                        .%(e => e.underground.not || collapse || f.invertory.has(Javelin) || (f.assigned.has(Bomb).not && f.free > 0 && e.armor <= maxStr(f.free - 1)))
                        .any
                    ) {
                        r :+= "Attack " ~ Dragon.elem
                    }

                    if (board.list(origin).has(Crystal) && maxStr(f.free) >= 3) {
                        r :+= "Smash " ~ Crystal.elem
                    }

                    if (board.list(origin).has(Chest)) {
                        r :+= "Take " ~ "Treasure".styled(f)
                    }

                    if (board.list(origin).of[DragonGem].any) {
                        r :+= "Collect " ~ "Dragon Gem".styled(Dragon)
                    }

                    if (r.any)
                        + StartEncounterAction(f).as("Encounter", dt.PerceptionK, dt.Arrow, r.comma)
                            .!(factions.of[Dragon.type].%(_.position.has(origin)).%(_.underground.not).%(_.armor > f.strength).any, "not enought strength to attack dragon")
                }

                assignStamina(f, KnightTurnAction(f), true, true, true)

                if (f.free > 0) {
                    + KnightShieldAction(f, KnightTurnAction(f))
                        .!(f.assigned.has(Shield))

                    if (f.assigned.has(Bow).not)
                        factions.of[Goblins.type].foreach { g =>
                            g.tribes.foreach { t =>
                                t.position.foreach { p =>
                                    if (board.visible(p, origin))
                                        + KnightBowAction(f, g, t.tribe, f.strength - 1)
                                            .!(f.strength < 2, "not enough strength")
                                }
                            }
                        }

                    if (f.invertory.has(EnchantedBow) && f.assigned.has(EnchantedBow).not)
                        factions.of[Dragon.type].foreach { e =>
                            e.position.foreach { p =>
                                if ((p.x == origin.x && (p.y - origin.y).abs <= 5) || (p.y == origin.y && (p.x - origin.x).abs <= 5))
                                    + KnightEnchantedBowAction(f, e, f.strength - 1)
                                        .!(f.strength < 2, "not enough strength")
                            }
                        }

                    if (f.assigned.has(Bomb).not)
                        + KnightBombMainAction(f)
                            .!(f.bombs <= 0, "no bombs")
                            .!(f.assigned.has(Bomb), "used bomb")
                            .!(Bearings.wnes.all(board.wall(f.position, _).not), "no walls")

                    if (f.invertory.has(PotionKit))
                        + KnightPotionKitAction(f)
                            .!(f.health >= 7, "full health")
                }

                f.stash.foreach { q =>
                    + KnightRevealTreasureAction(f, q)
                }

                if (board.list(f.position).has(Event).not)
                    + EndPlayerTurnAction(f).as("End Turn")

                ask(f).needOk

            case KnightAssignAction(f, s : Stat, then) =>
                f.assigned :+= s

                f.log("increased", s)

                then

            case KnightAssignAction(f, q, then) =>
                f.assigned :+= q

                f.reveal(q)

                f.log("used", q)

                then

            case KnightRevealTreasureAction(f, q) =>
                f.reveal(q)

                f.log("revealed", q)

                KnightTurnAction(f)

            case RecoverFromPoisionAction(f) =>
                f.log("recovered from", Poison, "and got", f.poison.hl, Stamina)

                f.poison = 0

                KnightTurnAction(f)

            case ClaimQuestAction(f, q, then) =>
                f.sidequests :-= q

                f.log("completed", q)

                f.score(q.grit)

                then

            case ClaimQuestsAction(f, l, then) if l.none =>
                then

            case ClaimQuestsAction(f, l, then) =>
                Ask(f).each(l)(q => ClaimQuestAction(f, q, ClaimQuestsAction(f, l :- q, then))).skip(then)

            case KnightBombMainAction(f) =>
                implicit val ask = builder

                Bearings.wnes.foreach { dir =>
                    var wall = board.wall(f.position, dir)

                    + KnightBombAction(f, dir)
                        .!(wall.not)
                }

                ask(f).cancel

            case KnightBombAction(f, dir) =>
                f.assigned :+= Bomb

                f.bombs -= 1

                board.bombed :+= (f.position, dir)

                board.rockslides = board.rockslides.but((f.position, dir)).but((f.position.add(dir), dir.reverse))

                f.log("bombed a wall", dir)

                ShowOpenEdgesAction(f, KnightTurnAction(f))

            case KnightBowAction(f, e, t, n) =>
                f.assigned :+= Bow

                f.log("shot", t, "with", Bow)

                e.adjustRage(1)

                ReducePopulationAction(e, t, n, ClaimQuestsAction(f, f.quests.of[EagleEyed.type], KnightTurnAction(f)))

            case KnightEnchantedBowAction(f, e : Dragon.type, n) =>
                f.assigned :+= EnchantedBow

                f.reveal(EnchantedBow)

                f.log("shot", e, "with", EnchantedBow)

                DragonDiscardPowersMainAction(e, min(3, f.strength - 1), ClaimQuestsAction(f, f.quests.of[EagleEyed.type], KnightTurnAction(f)))

           case DragonDiscardPowersMainAction(f, n, then) if f.powers.of[PowerCard].none =>
               Ask(f).add(then.as("No power cards"))

           case DragonDiscardPowersMainAction(f, n, then) =>
                implicit def convert(c : DragonCard, selected : Boolean) = selected.?(c.imgs).|(c.img)

                val l = f.powers.of[PowerCard]
                val k = min(n, l.num)

                XXSelectObjectsAction(f, l)
                    .withGroup(f, "discards", "Powers".styled(f))
                    .withRule(_.num(k))
                    .withThen(l => DragonDiscardPowersAction(f, l, then))(l => "Discard " ~ l./(_.elem).merge)

            case DragonDiscardPowersAction(f, l, then) =>
                f.powers = f.powers.diff(l)

                f.log("discarded", l)

                then

            case KnightShieldAction(f, then) =>
                f.assigned :+= Shield

                f.log("wielded", Shield)

                then

            case KnightPotionKitAction(f) =>
                f.discard(PotionKit)

                f.log("used", PotionKit)

                LightHealAction(f, KnightTurnAction(f))

            case KnightMoveAction(f, dir, map, _) =>
                if (map) {
                    f.assigned :+= AncientMap

                    f.log("used an", AncientMap)
                }

                f.log("moved", dir)

                f.position = f.position.add(dir)

                f.path :+= f.position

                val cell = board.get(f.position)

                f.dark = cell.is[HiddenTile] || cell == Emptiness

                val tokens = board.list(f.position)

                if (tokens.of[FlameWall].any) {
                    f.log("was burned by", "Flame Wall".styled(Dragon))

                    f.score(-5)
                }

                if (cell == Emptiness) {
                    board.set(f.position, Pending)

                    board.expand()

                    FillOpenEdgesAction(f, $(f.position), RevealTileAction(f, f.position, Some(dir), StartEncounterAction(f)))
                }
                else
                if (cell.is[HiddenTile])
                    RevealTileAction(f, f.position, Some(dir), StartEncounterAction(f))
                else
                if (tokens.has(Event) || factions.of[Goblins.type]./~(_.positions).has(f.position))
                    StartEncounterAction(f)
                else
                    KnightTurnAction(f)

            case StartEncounterAction(f) =>
                f.encountered :+= f.position

                f.log("started an", "Encounter".styled(f))

                EncounterTileAction(f)

            case EncounterTileAction(f) if board.list(f.position).has(Event) =>
                val tokens = board.list(f.position)

                board.remove(f.position, Event)

                StartEventAction(f)

            case StartEventAction(f) if events.none =>
                log("No more events")

                EncounterTileAction(f)

            case StartEventAction(f) if factions.has(Cave) =>
                Ask(Cave).each(events.take(3))(ChooseEventAction(Cave, f, _))

            case StartEventAction(f) =>
                val e = events.head

                events = events.drop(1)

                ExecuteEventAction(f, e)

            case ChooseEventAction(f : Cave.type, k, e) =>
                f.log("chose an event")

                events :-= e

                events = events.drop(2) ++ events.take(2)

                ExecuteEventAction(k, e)

            case ExecuteEventAction(f, e : Ambush) =>
                f.log("got into", e)

                Ask(f).add(ProcessAmbushAction(f, EncounterTileAction(f)).as("Ambush!")(e.img)).needOk

            case ExecuteEventAction(f, e : Fresh) =>
                f.log("found", e)

                Ask(f)
                    .add(FreshHealAction(f, EncounterTileAction(f)).as("Heal", dt.Stamina, dt.Arrow, "+1".hl, "Health".styled(f))(e.img).!(f.health >= 7).!(f.free <= 0))
                    .skip(EncounterTileAction(f))
                    .needOk

            case FreshHealAction(f, then) =>
                f.assigned :+= SetAside

                f.health += 1

                f.log("used", Stamina, "to heal", 1.hl, "Health".styled(f))

                then

            case ExecuteEventAction(f, e : Light.type) =>
                f.log("found", e)

                Ask(f)
                    .add(LightHealAction(f, EncounterTileAction(f)).as("Heal", dt.Discard, dt.Arrow, "+2".hl, "Health".styled(f))(e.img).!(f.health >= 7).!(f.free <= 0).!(f.stamina - f.lost - f.poison <= 2))
                    .skip(EncounterTileAction(f))
                    .needOk

            case LightHealAction(f, then) =>
                if (f.stamina > 2) {
                    f.lost += 1

                    f.log("lost", 1.hl, Stamina)
                }

                val h = f.health

                f.health += 2

                if (f.health >= 7)
                    f.health = 7

                f.log("healed", (f.health - h).hl, "Health".styled(f))

                then

            case ExecuteEventAction(f, e : CaveBread.type) =>
                f.log("found", e)

                Ask(f)
                    .add(CaveBreadAction(f, EncounterTileAction(f)).as("Gain", dt.Stamina)(e.img).!(f.stamina >= 7, "max"))
                    .bailout(EncounterTileAction(f).as("Skip"))
                    .needOk

            case CaveBreadAction(f, then) =>
                if (f.lost > 0) {
                    f.lost -= 1

                    f.log("recovered", 1.hl, Stamina)
                }
                else {
                    f.stamina += 1

                    f.log("gained", 1.hl, Stamina)
                }

                then

            case ExecuteEventAction(f, e : Rats) =>
                f.log("was attacked by", e)

                Ask(f)
                    .add(RatsAction(f, EncounterTileAction(f)).as("Crivens!")(e.img))
                    .needOk

            case RatsAction(f, then) =>
                f.score(-2)

                then

            case ExecuteEventAction(f, e : VantagePoint.type) =>
                f.log("found", e)

                val l = board.pattern(f.position, Around).%(board.get(_).is[HiddenTile])

                board.writeAll(f, l)

                if (l.none) {
                    f.log("had nothing to reveal")

                    Ask(f).add(EncounterTileAction(f).as("Nice view")(e.img)).needOk
                }
                else
                    VantagePointAction(f, l, EncounterTileAction(f))

            case VantagePointAction(f, l, then) if l.none =>
                then

            case VantagePointAction(f, l, then) =>
                Ask(f).each(l)(p => RevealTileAction(f, p, None, VantagePointAction(f, l.but(p), then)).as("Reveal", board.read(f, p).|("?").hl)(VantagePoint.img)).needOk

            case DeepAndDarkCaveAction(f, then) =>
                DrawOmensAction(f, 2, then)

            case ExecuteEventAction(f, e : DeepAndDarkCave) =>
                Cave.log("was", e)

                Ask(f)
                    .add(DeepAndDarkCaveAction(Cave, EncounterTileAction(f)).as("Whoa!")(e.img))
                    .needOk

            case ExecuteEventAction(f, e) =>
                f.log("got", e)

                EncounterTileAction(f)

            case EncounterTileAction(f) if board.list(f.position).has(Ambush) =>
                val tokens = board.list(f.position)

                f.log("walked into", Ambush)

                ProcessAmbushAction(f, EncounterTileAction(f))

            case EncounterTileAction(f) =>
                EncounterAttacksAction(f, $, EncounterCollectAction(f))

            case EncounterAttacksAction(f, l, then) =>
                implicit val ask = builder

                var skip = true
                var target = false

                if (factions.has(Goblins) && Goblins.tribes.exists(_.position.has(f.position))) {
                    skip = false
                    target = true

                    val tt = Goblins.tribes.%(_.position.has(f.position))

                    tt.%(_.strength >= f.strength).foreach { t =>
                        log("Warning:", f, "can't attack", t.tribe)
                    }

                    tt.%(_.strength < f.strength).foreach { t =>
                        + AttackAction(f, t.tribe, None, false, EncounterAttacksAction(f, t.tribe +: l, then))
                    }

                    if (f.invertory.has(Javelin))
                        tt.%(_.strength == f.strength).foreach { t =>
                            + AttackAction(f, t.tribe, Some(Javelin), false, EncounterAttacksAction(f, t.tribe +: l, then))
                        }
                }
                else
                if (factions.has(Dragon) && Dragon.position.has(f.position) && l.has(Dragon).not) {
                    val e = Dragon

                    if (collapse || e.underground.not || f.attacked.has(e).not)
                        target = true

                    if (!target) {
                        + AttackAction(f, e, None, false, EncounterAttacksAction(f, e +: l, then))
                            .!(true, "attacked this turn")
                    }
                    else {
                        if (e.underground.not)
                            skip = false

                        if (f.invertory.has(Javelin))
                            + AttackAction(f, e, Some(Javelin), f.strength + 1 == e.armor,  EncounterAttacksAction(f, e +: l, then))
                                .!(f.strength + 1 < e.armor, "not enough strength")
                                .!(f.strength > e.armor && (e.underground.not || collapse), "not needed")

                        if (e.underground && collapse.not) {
                            + AttackAction(f, e, Some(Bomb), f.strength == e.armor, EncounterAttacksAction(f, e +: l, then))
                                .!(f.strength < e.armor, "not enough strength")
                                .!(f.assigned.count(Bomb) >= 1, "used bomb")
                                .!(f.free <= 0, "no stamina for bomb")

                        }
                        else {
                            + AttackAction(f, e, None, f.strength == e.armor, EncounterAttacksAction(f, e +: l, then))
                                .!(f.strength < e.armor, "not enough strength")
                        }
                    }
                }

                if (board.list(f.position).has(Crystal)) {
                    val e = Cave

                    if (f.attacked.has(e).not)
                        target = true

                    + AttackAction(f, Crystal, None, false, EncounterAttacksAction(f, e +: l, then))
                        .!(f.strength < 3, "not enough strength")

                    if (f.invertory.has(Javelin))
                        + AttackAction(f, Crystal, Some(Javelin), false, EncounterAttacksAction(f, e +: l, then))
                            .!(f.strength + 1 < 3, "not enough strength")
                }

                if (target)
                    assignStamina(f, EncounterAttacksAction(f, l, then), false, false, true)

                ask(f).doneIf(skip)(then).needOkIf(target)

            case AttackAction(f, Crystal, q, _, then) =>
                q.foreach {
                    case q : Javelin.type => f.discard(q)
                    case _ =>
                }

                board.remove(f.position, Crystal)
                board.place(f.position, BrokenCrystal)

                smashed += 1

                f.log("smashed", Crystal, q./(("with", _)))

                f.score(2)

                then

            case AttackAction(f, t : Tribe, q, _, then) =>
                f.log("attacked", t, q./(("with", _)))

                q.foreach {
                    case q : Javelin.type => f.discard(q)
                    case _ =>
                }

                if (t.faction.revealed.has(Trap)) {
                    t.faction.revealed :-= Trap

                    t.faction.secrets.pile :+= Trap

                    t.faction.log("used", Trap, "to foil the attack")
                }

                var l : $[SideQuest] = $

                if (f.dark && f.quests.has(Fearless))
                    l :+= Fearless

                if (t.faction.tribe(t).population >= 3 && f.quests.has(StalwartGoblins))
                    l :+= StalwartGoblins

                if (t.faction.tribe(t).monsters.any && f.quests.has(Daring))
                    l :+= Daring

                ClaimQuestsAction(f, l, ScatterTribeAction(t.faction, t, then))

            case AttackAction(f, e : Dragon.type, q, _, then) =>
                f.attacked :+= e

                q.foreach {
                    case q : Javelin.type => f.discard(q)
                    case q : Bomb.type => f.assigned :+= q
                    case _ =>
                }

                if (f.strength + q.has(Javelin).??(1) < e.armor) {
                    f.log("could not attack", e)

                    then
                }
                else {
                    f.log("attacked", e, q./(("with", _)))

                    if (f.strength + q.has(Javelin).??(1) > e.armor)
                        AttackDamageAction(f, e, q, then)
                    else
                        Random(Pattern.die, AttackRollAction(f, e, q, _, then))
                }

            case AttackDamageAction(f, e : Dragon.type, q, then) =>
                e.health -= 1

                e.log("health reduced to", e.health.hl)

                if (e.health <= 0) {
                    f.log("killed", e)

                    Milestone(GameOverAction($(f)))
                }
                else
                    Ask(f)
                        .when(f.assigned.has(MightyAxe) && q.has(Bomb).not)(MightyAxeAction(f, e, q, then).!(f.stamina - f.lost - f.poison <= 2))
                        .skip(ClaimQuestsAction(f, f.quests.of[Daring.type].%(_ => current == f), then))

            case MightyAxeAction(f, e : Dragon.type, q, then) =>
                f.lost += 1

                f.assigned :-= MightyAxe

                f.log("discarded", Stamina, "to use", MightyAxe)

                AttackDamageAction(f, e, q, then)

            case AttackRollAction(f, e : Dragon.type, q, x, then) =>
                f.log("rolled", x, dt.Pattern(x))

                if (x.in(Saltire, Cross, Center))
                    AttackDamageAction(f, e, q, then)
                else {
                    f.log("missed")

                    then
                }

            case AttackAction(f, e, q, _, then) =>
                f.log("attacked", e, q./(("with", _)))

                then

            case EncounterCollectAction(f) =>
                implicit val ask = builder

                board.list(f.position).of[Chest.type].foreach { _ =>
                    + CollectChestAction(f)
                }

                val gems = board.list(f.position).of[DragonGem]

                gems.foreach { d =>
                    + CollectDragonGemAction(f, d)
                }

                if (gems.any)
                    if (f.free > 0)
                        if (f.assigned.has(Shield).not)
                            + KnightShieldAction(f, EncounterCollectAction(f))

                ask(f).done(KnightTurnAction(f))

            case CollectChestAction(f) =>
                board.remove(f.position, Chest)

                f.log("took", Chest)

                DrawTreasureAction(f)

            case CollectDragonGemAction(f, d) =>
                board.remove(f.position, d)

                f.log("collected", d)

                d.faction.gems :-= d.power

                Random(Pattern.die, CollectRollAction(f, _))

            case CollectRollAction(f, x) =>
                f.log("rolled", x, dt.Pattern(x))

                if (x.in(Saltire, Cross, Center))
                    f.score(-2)
                else
                    f.score(5)

                ClaimQuestsAction(f, f.quests.of[Cunning.type], EncounterCollectAction(f))

            case DrawTreasureAction(f) if treasures.none =>
                Force(DeclineTreasureAction(f))

            case DrawTreasureAction(f) if factions.has(Cave) =>
                Ask(Cave).each(treasures.take(2))(SelectTreasureAction(Cave, f, _))

            case DrawTreasureAction(f) =>
                val t = treasures.head

                treasures = treasures.drop(1)

                EvaluateTreasureAction(f, t)

            case SelectTreasureAction(f, k, t) =>
                treasures :-= t

                treasures = treasures.drop(1) ++ treasures.take(1)

                f.log("selected", "Treasure".styled(k))

                EvaluateTreasureAction(k, t)

            case EvaluateTreasureAction(f, t) =>
                Ask(f).add(TakeTreasureAction(f, t)).add(DeclineTreasureAction(f))

            case TakeTreasureAction(f, t) =>
                f.log("took", "Treasure".styled(f))

                t @@ {
                    case EquipmentTreasure(q) => f.stash :+= q
                    case _ =>
                }

                EncounterCollectAction(f)

            case DeclineTreasureAction(f) =>
                f.log("declined", "Treasure".styled(f))

                f.score(5)

                EncounterCollectAction(f)

            case ForceMoveMainAction(f, e : Knight.type, n, then) =>
                ForceMoveAction(f, e, 0, n, $(e.position), then)

            case ForceMoveMainAction(f, e : Dragon.type, n, then) =>
                ForceMoveAction(f, e, 0, n, e.position.$, then)

            case ForceMoveMainAction(f, t : Tribe, n, then) if t.faction.tribe(t).population <= 0 =>
                then

            case ForceMoveMainAction(f, t : Tribe, n, then) =>
                ForceMoveAction(f, t, 0, n, t.faction.tribe(t).position.$, then)

            case ForceMoveMainAction(f, TokenAt(t, p), n, then) =>
                ForceMoveAction(f, TokenAt(t, p), 0, n, $(p), then)

            case ForceMoveAction(f, TokenAt(t, p), k, n, l, then) if k >= n && factions./~(_.positions).has(p) =>
                board.remove(p, t)

                board.place(l(0), t)

                f.log("could not move", t, "to a player")

                ForceMoveAction(f, TokenAt(t, l(0)), 0, n, l.take(1), then)

            case ForceMoveAction(f, e, k, n, l, then) if k >= n =>
                then

            case ForceMoveAction(f, e : Knight.type, k, n, l, then) =>
                implicit val ask = builder

                val others = factions.but(e)./~(_.positions)

                val p = e.position

                Bearings.wnes.foreach { dir =>
                    val dest = p.add(dir)

                    + ForceMoveStepAction(f, e, dir, k + 1, n, l :+ dest, then)
                        .!(l.has(dest))
                        .!(board.get(dest).is[Emptiness.type])
                        .!(board.get(dest).is[HiddenTile])
                        .!(board.wall(p, dir))
                        .!(others.has(dest))
                }

                ask(f).done(then).needOk

            case ForceMoveStepAction(f, e : Knight.type, dir, k, n, l, then) =>
                e.position = e.position.add(dir)

                f.log("moved", e, dir)

                var nn = n

                val tokens = board.list(e.position)

                if (tokens.of[FlameWall].any) {
                    e.log("was burned by", "Flame Wall".styled(Dragon))

                    e.score(-5)
                }

                if (tokens.has(Event)) {
                    if (k < n) {
                        e.log("stopped due to", Event)
                        nn = k
                    }
                }

                ForceMoveAction(f, e, k, nn, l, then)

            case ForceMoveAction(f, e : Dragon.type, k, n, l, then) =>
                implicit val ask = builder

                val others = factions.but(e)./~(_.positions)

                val p = e.position.get

                Bearings.wnes.foreach { dir =>
                    val dest = p.add(dir)

                    + ForceMoveStepAction(f, e, dir, k + 1, n, l :+ dest, then)
                        .!(l.has(dest))
                        .!(board.get(dest).is[Emptiness.type])
                        .!(e.underground.not && board.get(dest).is[HiddenTile])
                        .!(e.underground.not && board.wall(p, dir))
                        .!(others.has(dest))
                }

                ask(f).done(then).needOk

            case ForceMoveStepAction(f, e : Dragon.type, dir, k, n, l, then) =>
                e.position = e.position./(_.add(dir))

                f.log("moved", e, dir)

                ForceMoveAction(f, e, k, n, l, then)

            case ForceMoveAction(f, t : Tribe, k, n, l, then) if t.faction.tribe(t).position.none =>
                then

            case ForceMoveAction(f, t : Tribe, k, n, l, then) =>
                implicit val ask = builder

                val others = factions.but(t.faction)./~(_.positions)

                val p = t.faction.tribe(t).position.get

                Bearings.wnes.foreach { dir =>
                    val dest = p.add(dir)

                    + ForceMoveStepAction(f, t, dir, k + 1, n, l :+ dest, then)
                        .!(l.has(dest))
                        .!(board.get(dest).is[Emptiness.type])
                        .!(board.wall(p, dir))
                        .!(others.has(dest))
                }

                ask(f).done(then).needOk

            case ForceMoveStepAction(f, t : Tribe, dir, k, n, l, then) =>
                t.faction.tribe(t).position = t.faction.tribe(t).position./(_.add(dir))

                f.log("moved", t, dir)

                if (board.list(t.faction.tribe(t).position.get).of[FlameWall].any) {
                    log(t, "was burned by", "Flame Wall".styled(Dragon))

                    ReducePopulationAction(t.faction, t, 1, ForceMoveAction(f, t, k, n, l, then))
                }
                else
                    ForceMoveAction(f, t, k, n, l, then)

            case ForceMoveAction(f, TokenAt(t, p), k, n, l, then) =>
                implicit val ask = builder

                val others = factions./~(_.positions)

                Bearings.wnes.foreach { dir =>
                    val dest = p.add(dir)

                    + ForceMoveStepAction(f, TokenAt(t, p), dir, k + 1, n, l :+ dest, then)
                        .!(l.has(dest))
                        .!(board.get(dest).is[Emptiness.type])
                        .!(board.wall(p, dir))
                }

                ask(f)
                    .when(t == Chest)(HighlightChestAt(p))
                    .doneIf(others.has(p).not || k == 0)(then)
                    .needOk
                    .bailw(ForceMoveAction(f, TokenAt(t, l(0)), 0, n, l.take(1), then)) {
                        board.remove(p, t)

                        board.place(l(0), t)

                        f.log("could not move", t, "to another player")
                    }

            case ForceMoveStepAction(f, TokenAt(t, p), dir, k, n, l, then) =>
                board.remove(p, t)

                board.place(p.add(dir), t)

                f.log("moved", t, dir)

                ForceMoveAction(f, TokenAt(t, p.add(dir)), k, n, l, then)

            case ForceMoveStraightMainAction(f, k : Knight.type, n, then) =>
                implicit val ask = builder

                val others = factions.but(k)./~(_.positions)

                Bearings.wnes.foreach { dir =>
                    var p = k.position
                    var d = 0

                    val stop = new scala.util.control.Breaks

                    stop.breakable {
                        while (d < n) {
                            if (board.wall(p, dir))
                                stop.break()

                            val dest = p.add(dir)

                            if (others.has(dest))
                                stop.break()

                            if (board.get(dest).is[HiddenTile])
                                stop.break()

                            if (board.get(dest) == Emptiness)
                                stop.break()

                            p = dest

                            d += 1

                            if (board.list(dest).has(Event))
                                stop.break()
                        }
                    }

                    + ForceMoveStraightAction(f, k, dir, d, then)
                        .!(d == 0)
                }

                ask(f).skip(then).needOk

            case ForceMoveStraightAction(f, e : Knight.type, dir, d, then) =>
                ForceMoveStraightStepAction(f, e, dir, d, then)

            case ForceMoveStraightStepAction(f, e : Knight.type, dir, 0, then) =>
                then

            case ForceMoveStraightStepAction(f, e : Knight.type, dir, d, then) =>
                f.log("moved", e, dir)

                e.position = e.position.add(dir)

                val tokens = board.list(e.position)

                if (tokens.of[FlameWall].any) {
                    e.log("was burned by", "Flame Wall".styled(Dragon))

                    e.score(-5)
                }

                ForceMoveStraightStepAction(f, e, dir, d - 1, then)

            case RevealTileAction(f, p, dir, then) =>
                val h = board.get(p).as[HiddenTile].get

                board.set(p, Revealing)

                board.write(f, p, "")

                revealed += 1

                var l = Bearings.all.drop(revealed % 4) ++ Bearings.all.take(revealed % 4)

                l = l.distinctBy(b => h.tile.walls.rotate(b))

                dir.foreach { dir =>
                    if (board.wall(p, dir.reverse).not)
                        l = l.%(b => h.tile.walls.rotate(b).wall(dir.reverse).not).some.|(l)
                }

                if (l.num > 1) {
                    val access = board.accessible

                    l = l.%(b => {
                        val w = h.tile.walls.rotate(b)
                        Bearings.all.exists(exit => w.wall(exit).not && board.wall(p, exit).not && {
                            val e = p.add(exit)
                            access(e.x + board.center.x)(e.y + board.center.y)
                        })
                    }).some.|(l)
                }

                Ask(f).each(l)(OrientTileAction(f, p, h, _, then))

            case OrientTileAction(f, p, h, dir, then) =>
                board.set(p, Explored(h.tile.name, h.tile.walls.rotate(dir), dir, |(h)))

                h.tokens.foreach { t =>
                    if (t != Ambush || factions.of[Knight.type].exists(_.position == p))
                        board.place(p, t)
                }

                f.log("revealed a dark tile")

                f.as[Knight.type].foreach(_.score(1))

                f.as[Dragon.type].foreach(_.events += h.tokens.count(Event))

                ShowOpenEdgesAction(f, then)

            case a => performInternalPart2(a, soft)
        }
    }

    def performInternalPart2(a : Action, soft : Void) : Continue = {
        implicit val action = a

        goblinDispatchPart2(soft)(this).lift(a) match {
            case Some(c) => c
            case None    => performInternalPart3(a, soft)
        }
    }

    def performInternalPart3(a : Action, soft : Void) : Continue = {
        implicit val action = a

        dragonDispatchPart3(soft)(this).lift(a) match {
            case Some(c) => c
            case None    => caveTailPart3(a, soft)
        }
    }

    protected def caveTailPart3(a : Action, soft : Void) : Continue = {
        implicit val action = a

        (caveTailCaveDispatchPart3(soft)(this).orElse(thiefCaveTailDispatchPart3(soft)(this)).orElse(boardTailDispatchPart3(soft)(this)))(a)
    }


}
