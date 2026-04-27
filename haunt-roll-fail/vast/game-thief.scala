// FILE: vast/game-thief.scala
// VERSION: 0.1.1
// START_MODULE_CONTRACT
// PURPOSE: Isolate the partial Thief/5p runtime support from the monolithic Vast action dispatcher.
// SCOPE: Own Thief setup helpers, basic turn flow, movement, Loot/Pick Lock, carried/stashed loot, and Thief verification markers.
// DEPENDS: vast.game, vast.rules(thief), hrf.base, hrf.logger, hrf.ui
// LINKS: M-VAST-THIEF-SUPPORT, M-VAST-GAME, M-VAST-RULES, M-VAST-UI, M-VAST-HOST, V-M-VAST-THIEF-SUPPORT
// ROLE: RUNTIME
// MAP_MODE: LOCALS
// END_MODULE_CONTRACT
//
// START_MODULE_MAP
// GameThiefSupport - helper dispatch and state transitions for the current partial Thief checkpoint
// END_MODULE_MAP
//
// START_CHANGE_SUMMARY
// LAST_CHANGE: v0.1.1 - `thiefCaveTailDispatchPart3` bridges Thief cases in `Game.caveTailPart3` chain (Cave→Thief→Board PFs). v0.1.0 extracted Thief helpers from Game.
// END_CHANGE_SUMMARY
package vast

import hrf.colmat._
import hrf.elem._
import hrf.logger._
import vast.elem._

trait GameThiefSupport { self : Game =>
    // START_BLOCK_THIEF_SUPPORT_CHECKPOINT
    // PURPOSE: Keep the partial Thief/5p implementation isolated from the large performInternal action match.
    // STATUS: partial checkpoint. Implemented here: setup, basic stats/action cubes, movement, dark-tile reveal choice, Loot, Pick Lock, carried/stashed loot, and six-stash victory.
    // PENDING: Pickpocket, Backstab, Hide Loot, full death rewards, upgrade board, and full cross-faction effects from Knight/Goblins/Dragon/Cave.
    // JVM_LIMIT_NOTE: performInternal is near the JVM per-method bytecode limit. Do not add new Thief or cross-faction cases directly there; route through small helper dispatch methods or extract whole faction sections first.
    // LINKS: M-VAST-THIEF-SUPPORT, V-M-VAST-THIEF-SUPPORT, rules/thief.xml
    protected def logThiefMarker(scope : String, block : String, message : String) : Unit =
        +++("[VastThief][" + scope + "][" + block + "] " + message)

    protected def setupThief(f : Thief.type) : ForcedAction = {
        implicit val g : Game = this

        states += f -> new ThiefPlayer(this, f)
        f.position = board.entrance
        f.log("started at the", Entrance)
        SetupNextAction
    }

    protected def startThiefTurn(f : Thief.type) : ForcedAction = {
        implicit val g : Game = this

        if (f.statsAssigned.not) {
            f.movement = 2
            f.stealth = 3
            f.thievery = 4
            f.statsAssigned = true
        }
        f.actionCubes = f.thievery
        f.targeted = $
        f.path = $
        logThiefMarker("turn", "BLOCK_THIEF_TURN", "movement=" + f.movement + " stealth=" + f.stealth + " thievery=" + f.thievery + " carried=" + f.carried.num + " stashed=" + f.stashed)
        ThiefTurnAction(f)
    }

    protected def thiefTurn(f : Thief.type) : Continue = {
        implicit val g : Game = this
        implicit val ask = builder

        if (f.statsAssigned.not)
            List((2, 3, 4), (2, 4, 3), (3, 2, 4), (3, 4, 2), (4, 2, 3), (4, 3, 2)).foreach { case (m, s, t) =>
                + ThiefAssignStatsAction(f, m, s, t)
            }
        else {
            board.list(f.position).of[Chest.type].foreach(t => + ThiefLootAction(f, t, 1).!(f.actionCubes < 1, "no action cubes"))
            board.list(f.position).of[DragonGem].foreach { t =>
                + ThiefLootAction(f, t, 1).!(f.actionCubes < 1, "no action cubes")
                + ThiefLootAction(f, t, 2).!(f.actionCubes < 2, "no action cubes")
            }
            if (board.list(f.position).has(Vault))
                1.to(3).foreach(cubes => + ThiefPickLockAction(f, cubes).!(f.actionCubes < cubes, "no action cubes"))

            Bearings.wnes.foreach { dir =>
                val dest = f.position.add(dir)
                val cell = board.get(dest)
                + ThiefMoveAction(f, dir, cell.is[HiddenTile])
                    .!(f.moves >= f.movement, "no movement")
                    .!(cell == Emptiness, "empty")
                    .!(board.wall(f.position, dir), "wall")
            }
        }

        + EndPlayerTurnAction(f).as("End Turn")
        ask(f).needOk
    }

    protected def spendThiefCubes(f : Thief.type, n : Int) {
        implicit val g : Game = this

        f.actionCubes = max(0, f.actionCubes - n)
    }

    protected def assignThiefStats(f : Thief.type, movement : Int, stealth : Int, thievery : Int) : ForcedAction = {
        implicit val g : Game = this

        f.movement = movement
        f.stealth = stealth
        f.thievery = thievery
        f.statsAssigned = true
        f.actionCubes = thievery
        f.log("assigned stats", "Movement".hh, movement.hl, "Stealth".hh, stealth.hl, "Thievery".hh, thievery.hl)
        ThiefTurnAction(f)
    }

    protected def moveThief(f : Thief.type, dir : Bearing) : Continue = {
        implicit val g : Game = this

        f.position = f.position.add(dir)
        f.path :+= f.position
        f.log("moved", dir)

        if (board.get(f.position).is[HiddenTile])
            Ask(f)
                .add(RevealTileAction(f, f.position, Some(dir), ThiefTurnAction(f)).as("Reveal", board.read(f, f.position).|("?").hl))
                .add(ThiefKeepDarkAction(f))
                .needOk
        else
        if (f.position == board.entrance && f.carried.any)
            stashThiefLoot(f)
        else
            ThiefTurnAction(f)
    }

    protected def lootThief(f : Thief.type, t : Token, cubes : Int) : Continue = {
        implicit val g : Game = this

        spendThiefCubes(f, cubes)
        board.remove(f.position, t)
        f.carried :+= t
        f.log("took", t)

        if (t.is[DragonGem] && cubes == 1)
            Random(Pattern.die, ThiefLootRollAction(f, t, _))
        else
        if (f.position == board.entrance)
            stashThiefLoot(f)
        else
            ThiefTurnAction(f)
    }

    protected def resolveThiefLootRoll(f : Thief.type, t : Token, x : Pattern) : ForcedAction = {
        implicit val g : Game = this

        f.log("rolled", x, dt.Pattern(x))
        if (x == Center) killThief(f, ThiefTurnAction(f)) else if (f.position == board.entrance) stashThiefLoot(f) else ThiefTurnAction(f)
    }

    protected def thiefRollSuccess(cubes : Int, x : Pattern) : Boolean =
        cubes match {
            case 1 => x.in(Center, Cross, Saltire)
            case 2 => x != Around
            case _ => true
        }

    protected def pickLockThief(f : Thief.type, cubes : Int) : Continue = {
        implicit val g : Game = this

        spendThiefCubes(f, cubes)
        if (cubes >= 3) pickLockThiefSuccess(f) else Random(Pattern.die, ThiefPickLockRollAction(f, cubes, _))
    }

    protected def resolveThiefPickLockRoll(f : Thief.type, cubes : Int, x : Pattern) : ForcedAction = {
        implicit val g : Game = this

        f.log("rolled", x, dt.Pattern(x))
        if (thiefRollSuccess(cubes, x)) pickLockThiefSuccess(f) else { f.log("failed to pick", Vault); ThiefTurnAction(f) }
    }

    protected def pickLockThiefSuccess(f : Thief.type) : ForcedAction = {
        implicit val g : Game = this

        board.remove(f.position, Vault)
        f.carried :+= Chest
        f.log("opened", Vault)
        logThiefMarker("loot", "BLOCK_THIEF_LOOT_STASH", "carried=" + f.carried.num)
        if (f.position == board.entrance) stashThiefLoot(f) else ThiefTurnAction(f)
    }

    protected def stashThiefLoot(f : Thief.type) : ForcedAction = {
        implicit val g : Game = this

        val n = f.carried.num
        f.carried.foreach { case DragonGem(d, power) => d.gems :-= power; case _ => }
        f.carried = $
        f.stashed += n
        f.lootDrop = 3
        f.log("stashed", n.hl, "Treasure".s(n).styled(f))
        logThiefMarker("loot", "BLOCK_THIEF_LOOT_STASH", "stashed=" + f.stashed)
        if (f.stashed >= 6) GameOverAction($(f)) else ThiefTurnAction(f)
    }

    protected def killThief(f : Thief.type, then : ForcedAction) : ForcedAction = {
        implicit val g : Game = this

        f.carried.foreach(board.place(f.position, _))
        f.carried = $
        f.position = board.entrance
        f.actionCubes = 0
        f.log("was killed")
        then
    }

    protected def performThief(a : ThiefAction) : Continue = a match {
        case ThiefAssignStatsAction(f, movement, stealth, thievery) => assignThiefStats(f, movement, stealth, thievery)
        case ThiefMoveAction(f, dir, _) => moveThief(f, dir)
        case ThiefKeepDarkAction(f) => ThiefTurnAction(f)
        case ThiefLootAction(f, t, cubes) => lootThief(f, t, cubes)
        case ThiefLootRollAction(f, t, x) => resolveThiefLootRoll(f, t, x)
        case ThiefPickLockAction(f, cubes) => pickLockThief(f, cubes)
        case ThiefPickLockRollAction(f, cubes, x) => resolveThiefPickLockRoll(f, cubes, x)
        case _ => ThiefTurnAction(Thief)
    }

    protected def thiefCaveTailDispatchPart3(soft : Void)(implicit g : Game) : PartialFunction[Action, Continue] = {
            case ContinuePlayerTurnAction(f : Thief.type) =>
                startThiefTurn(f)

            case ThiefTurnAction(f) =>
                thiefTurn(f)

            case a : ThiefAction =>
                performThief(a)

    }

    // END_BLOCK_THIEF_SUPPORT_CHECKPOINT
}
