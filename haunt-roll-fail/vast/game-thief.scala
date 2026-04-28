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

    protected def canTargetThief(f : Thief.type, target : Faction) : Boolean = {
        implicit val g : Game = this
        target match {
            case e : Knight.type  => f.effectiveStealth > e.perception
            case e : Goblins.type => f.effectiveStealth > e.tribes./(_.population).sum + 1
            case e : Dragon.type  => f.effectiveStealth > e.armor
            case _ : Cave.type    => true
            case _                => false
        }
    }

    protected def sameSpace(f : Thief.type, target : Faction) : Boolean = {
        implicit val g : Game = this
        target match {
            case e : Knight.type  => e.position == f.position
            case e : Goblins.type => e.tribes.exists(_.position.has(f.position))
            case e : Dragon.type  => e.position.has(f.position)
            case _ : Cave.type    => true
            case _                => false
        }
    }

    protected def canPickpocket(f : Thief.type, target : Faction) : Boolean = {
        implicit val g : Game = this
        target match {
            case e : Knight.type  => e.stash.any
            case e : Goblins.type => e.hand.any
            case e : Dragon.type  => e.greed.available
            case _ : Cave.type    => true
            case _                => false
        }
    }

    protected def setupThief(f : Thief.type) : ForcedAction = {
        implicit val g : Game = this

        states += f -> new ThiefPlayer(this, f)
        f.position = board.entrance
        f.log("started at the", Entrance)
        SetupNextAction
    }

    protected def startThiefTurn(f : Thief.type) : ForcedAction = {
        implicit val g : Game = this

        if (f.dead) {
            f.dead = false
            f.position = board.entrance
            f.log("respawned at", Entrance)
        }

        if (f.statsAssigned.not) {
            f.movement = 2
            f.stealth = 3
            f.thievery = 4
            f.statsAssigned = true
        }
        f.actionCubes = f.thievery
        f.targeted = $
        f.path = $
        f.usedStickyFingers = false
        f.usedEvasion = false
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

            if (f.lootDrop > 0 && f.actionCubes > 0)
                1.to(min(f.actionCubes, f.lootDrop)).foreach(cubes => + ThiefHideLootAction(f, cubes))

            factions.but(f).foreach { target =>
                if (canTargetThief(f, target) && sameSpace(f, target) && canPickpocket(f, target) && f.targeted.has(target).not)
                    1.to(min(f.actionCubes, 3)).foreach(cubes => + ThiefPickpocketAction(f, target, cubes))
            }

            factions.but(f).but(Cave).foreach { target =>
                if (canTargetThief(f, target) && sameSpace(f, target) && f.targeted.has(target).not)
                    1.to(min(f.actionCubes, 3)).foreach(cubes => + ThiefBackstabAction(f, target, cubes))
            }

            Bearings.wnes.foreach { dir =>
                val dest = f.position.add(dir)
                val cell = board.get(dest)
                + ThiefMoveAction(f, dir, cell.is[HiddenTile])
                    .!(f.moves >= f.movement, "no movement")
                    .!(cell == Emptiness, "empty")
                    .!(board.wall(f.position, dir), "wall")

                // Climb through wall
                if (board.wall(f.position, dir) && cell != Emptiness) {
                    val climbCost = if (f.upgrades.has(ClimbingGear)) 1 else 2
                    + ThiefClimbAction(f, dir, climbCost, cell.is[HiddenTile])
                        .!(f.moves >= f.movement, "no movement")
                        .!(f.actionCubes < climbCost, "no action cubes")
                }
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

    protected def resolveThiefLootRoll(f : Thief.type, t : Token, x : Pattern) : Continue = {
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

    protected def resolveThiefPickLockRoll(f : Thief.type, cubes : Int, x : Pattern) : Continue = {
        implicit val g : Game = this

        f.log("rolled", x, dt.Pattern(x))
        if (thiefRollSuccess(cubes, x)) pickLockThiefSuccess(f) else { f.log("failed to pick", Vault); ThiefTurnAction(f) }
    }

    protected def pickLockThiefSuccess(f : Thief.type) : Continue = {
        implicit val g : Game = this

        board.remove(f.position, Vault)
        f.carried :+= Chest
        f.log("opened", Vault)
        logThiefMarker("loot", "BLOCK_THIEF_LOOT_STASH", "carried=" + f.carried.num)
        if (f.position == board.entrance) stashThiefLoot(f) else ThiefTurnAction(f)
    }

    protected def stashThiefLoot(f : Thief.type) : Continue = {
        implicit val g : Game = this

        val n = f.carried.num
        f.carried.foreach { case DragonGem(d, power) => d.gems :-= power ; case _ => }
        f.carried = $
        f.stashed += n
        f.lootDrop = 3
        f.log("stashed", n.hl, "Treasure".s(n).styled(f))
        logThiefMarker("loot", "BLOCK_THIEF_LOOT_STASH", "stashed=" + f.stashed)

        if (f.stashed >= 6)
            GameOverAction($(f))
        else
            chooseUpgrade(f)
    }

    protected def chooseUpgrade(f : Thief.type) : Continue = {
        implicit val g : Game = this
        implicit val ask = builder

        val specials = $(LockPickingKit, ClimbingGear, HandCrossbow, StickyFingers, UnnaturalEvasion).diff(f.upgrades)
        val stats = $("Movement", "Stealth", "Thievery")./(StatBoost).%( {
            case StatBoost("Movement") => f.movement < 5
            case StatBoost("Stealth")  => f.stealth < 5
            case StatBoost("Thievery") => f.thievery < 5
            case _ => false
        })
        val available = specials ++ stats

        if (available.any && f.stashed > f.upgrades.num) {
            available.foreach(u => + ThiefStashChoiceAction(f, Some(u)))
            + ThiefStashChoiceAction(f, None)
            ask(f).needOk
        }
        else
            ThiefTurnAction(f)
    }

    protected def applyUpgrade(f : Thief.type, upgrade : |[ThiefUpgrade]) : ForcedAction = {
        implicit val g : Game = this

        upgrade match {
            case None => ThiefTurnAction(f)
            case Some(u) =>
                f.upgrades :+= u
                f.log("gained upgrade", u)
                u match {
                    case StatBoost("Movement") => f.movement += 1
                    case StatBoost("Stealth")  => f.stealth += 1
                    case StatBoost("Thievery") => f.thievery += 1
                    case _ =>
                }
                ThiefTurnAction(f)
        }
    }

    protected def killThief(f : Thief.type, then : ForcedAction, killer : |[Faction] = None) : ForcedAction = {
        implicit val g : Game = this

        f.carried.foreach(board.place(f.position, _))
        f.carried = $
        f.dead = true
        f.actionCubes = 0
        f.log("was killed")

        killer.foreach { k =>
            val reward = f.lootDrop
            k match {
                case e : Knight.type =>
                    e.grit += reward
                    e.log("gained", reward.hl, "Grit from killing", f)
                case e : Goblins.type =>
                    e.rage += reward
                    e.log("gained", reward.hl, "Rage from killing", f)
                case e : Dragon.type =>
                    1.to(reward).foreach(_ => e.powers.of[PowerCard].shuffle.starting.foreach(c => {}))
                    e.log("gained Power cards from killing", f)
                case _ =>
            }
        }

        then
    }

    protected def hideLootThief(f : Thief.type, cubes : Int) : ForcedAction = {
        implicit val g : Game = this

        spendThiefCubes(f, cubes)
        f.lootDrop = max(0, f.lootDrop - cubes)
        f.log("hid loot, drop level now", f.lootDrop.hl)
        ThiefTurnAction(f)
    }

    protected def pickpocketThief(f : Thief.type, target : Faction, cubes : Int) : Continue = {
        implicit val g : Game = this

        spendThiefCubes(f, cubes)
        f.targeted :+= target
        f.log("attempted to pickpocket", target)

        if (cubes >= 3)
            pickpocketThiefSuccess(f, target)
        else
            Random(Pattern.die, ThiefPickpocketRollAction(f, target, cubes, _))
    }

    protected def resolveThiefPickpocketRoll(f : Thief.type, target : Faction, cubes : Int, x : Pattern) : ForcedAction = {
        implicit val g : Game = this

        f.log("rolled", x, dt.Pattern(x))
        if (thiefRollSuccess(cubes, x))
            pickpocketThiefSuccess(f, target)
        else {
            f.log("failed to pickpocket", target)
            ThiefTurnAction(f)
        }
    }

    protected def pickpocketThiefSuccess(f : Thief.type, target : Faction) : ForcedAction = {
        implicit val g : Game = this

        target match {
            case e : Cave.type =>
                f.carried :+= Chest
                f.log("stole", Chest, "from", e)

            case e : Knight.type =>
                e.stash.shuffle.starting.foreach { t =>
                    e.stash :-= t
                    f.carried :+= Chest
                    f.log("stole a treasure from", e)
                }

            case e : Goblins.type =>
                e.hand.shuffle.starting.foreach { s =>
                    e.hand :-= s
                    f.log("stole a secret from", e)
                }

            case e : Dragon.type =>
                if (e.greed.available) {
                    e.greed.reduce()
                    e.wakefulness += 1
                    f.log("moved Sloth to Greed on", e)
                }

            case _ =>
        }

        logThiefMarker("loot", "BLOCK_THIEF_LOOT_STASH", "pickpocket=" + target.short + " carried=" + f.carried.num)
        ThiefTurnAction(f)
    }

    protected def backstabThief(f : Thief.type, target : Faction, cubes : Int) : ForcedAction = {
        implicit val g : Game = this

        spendThiefCubes(f, cubes)
        f.targeted :+= target

        target match {
            case e : Knight.type =>
                val damage = cubes match { case 1 => 1 ; case 2 => 3 ; case _ => 5 }
                e.grit = max(0, e.grit - damage)
                f.log("backstabbed", e, "for", damage.hl, "Grit")

            case e : Goblins.type =>
                val damage = cubes match { case 1 => 1 ; case 2 => 2 ; case _ => 3 }
                e.tribes.sortBy(-_.population).take(1).foreach { t =>
                    t.population = max(0, t.population - damage)
                }
                f.log("backstabbed", e, "for", damage.hl, "Population")

            case e : Dragon.type =>
                val discard = cubes match { case 1 => 1 ; case 2 => 2 ; case _ => 3 }
                val discarded = e.powers.of[PowerCard].shuffle.take(discard)
                discarded.foreach(c => e.powers :-= c)
                f.log("backstabbed", e, ", discarded", discard.hl, "Power card".s(discard))

            case _ =>
        }

        ThiefTurnAction(f)
    }

    protected def climbThief(f : Thief.type, dir : Bearing, cubes : Int) : Continue = {
        implicit val g : Game = this

        spendThiefCubes(f, cubes)
        f.log("climbed", dir, "for", cubes.hl, "cube".s(cubes))
        moveThief(f, dir)
    }

    protected def tryEvasion(f : Thief.type, attacker : Faction, then : ForcedAction, onFail : ForcedAction) : Continue = {
        implicit val g : Game = this

        if (f.upgrades.has(UnnaturalEvasion) && f.usedEvasion.not) {
            f.usedEvasion = true
            Random(Pattern.die, ThiefEvasionRollAction(f, attacker, then, _))
        }
        else
            onFail
    }

    protected def resolveEvasionRoll(f : Thief.type, attacker : Faction, then : ForcedAction, x : Pattern) : Continue = {
        implicit val g : Game = this

        f.log("evasion roll", x, dt.Pattern(x))
        if (thiefRollSuccess(1, x)) {
            f.log("evaded attack from", attacker)
            then
        }
        else
            killThief(f, then)
    }

    protected def performThief(a : ThiefAction) : Continue = a match {
        case ThiefAssignStatsAction(f, movement, stealth, thievery) => assignThiefStats(f, movement, stealth, thievery)
        case ThiefMoveAction(f, dir, _) => moveThief(f, dir)
        case ThiefKeepDarkAction(f) => ThiefTurnAction(f)
        case ThiefLootAction(f, t, cubes) => lootThief(f, t, cubes)
        case ThiefLootRollAction(f, t, x) => resolveThiefLootRoll(f, t, x)
        case ThiefPickLockAction(f, cubes) => pickLockThief(f, cubes)
        case ThiefPickLockRollAction(f, cubes, x) => resolveThiefPickLockRoll(f, cubes, x)
        case ThiefHideLootAction(f, cubes) => hideLootThief(f, cubes)
        case ThiefPickpocketAction(f, target, cubes) => pickpocketThief(f, target, cubes)
        case ThiefPickpocketRollAction(f, target, cubes, x) => resolveThiefPickpocketRoll(f, target, cubes, x)
        case ThiefBackstabAction(f, target : Faction, cubes) => backstabThief(f, target, cubes)
        case ThiefClimbAction(f, dir, cubes, _) => climbThief(f, dir, cubes)
        case ThiefStashChoiceAction(f, upgrade) => applyUpgrade(f, upgrade)
        case ThiefEvasionRollAction(f, attacker, then, x) => resolveEvasionRoll(f, attacker, then, x)
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
