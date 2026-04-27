// FILE: vast/game-setup.scala
// VERSION: 0.1.0
// START_MODULE_CONTRACT
// PURPOSE: Isolate Vast startup, initial tile shuffle, and faction setup routing from the main action dispatcher.
// SCOPE: Own StartSetupAction, initial tile grouping, setup sequencing, faction state construction, and init completion helpers.
// DEPENDS: vast.game, vast.meta, hrf.base, hrf.logger
// LINKS: M-VAST-GAME, M-VAST-META, M-VAST-THIEF-SUPPORT, V-M-VAST-GAME, V-M-VAST-META
// ROLE: RUNTIME
// MAP_MODE: LOCALS
// END_MODULE_CONTRACT
//
// START_MODULE_MAP
// GameSetupSupport - behavior-preserving setup helpers used by Game.performInternal routing cases
// END_MODULE_MAP
//
// START_CHANGE_SUMMARY
// LAST_CHANGE: v0.1.0 - Extracted setup helpers to reduce bytecode pressure inside Game.performInternal.
// END_CHANGE_SUMMARY
package vast

import hrf.colmat._

trait GameSetupSupport { self : Game with GameThiefSupport =>
    // START_BLOCK_GAME_SETUP_DISPATCH
    // PURPOSE: Keep startup/setup case bodies out of performInternal while preserving the same action sequence.
    // JVM_LIMIT_NOTE: This extraction reduces the bytecode size of Game.performInternal without changing public action classes or serialization shapes.
    // LINKS: M-VAST-GAME, M-VAST-META, M-VAST-THIEF-SUPPORT, V-M-VAST-GAME
    protected def startSetup() : Continue = {
        if (setup.has(Thief))
            logThiefMarker("enable", "BLOCK_ENABLE_THIEF_FIFTH_PLAYER", "setup=" + setup./(_.short).mkString(","))
        Shuffle3(Tiles.ambushes ++ Tiles.events ++ Tiles.treasures, Tiles.crystals, setup.has(Thief).??(Tiles.vaults), (m, c, v) => InitialTilesAction(m, c, v))
    }

    protected def initialTiles(m : $[HiddenTile], c : $[HiddenTile], v : $[HiddenTile]) : Continue = {
        log("Shuffled tiles once")

        board.expand()

        board.cells(1)(0) = m(0)
        board.cells(2)(1) = m(1)
        board.cells(1)(2) = m(2)
        board.cells(0)(1) = m(3)

        board.expand()

        Shuffle3(m.drop(4).take(11) ++ c.take(3) ++ v.take(2), m.drop(4).drop(11).take(11) ++ c.drop(3).take(3) ++ v.drop(2).take(2), m.drop(4).drop(11+11) ++ c.drop(3+3) ++ v.drop(2+2), (m, c, v) => GroupedTilesAction(m, c, v))
    }

    protected def groupedTiles(l1 : $[HiddenTile], l2 : $[HiddenTile], l3 : $[HiddenTile]) : ForcedAction = {
        tiles = l1 ++ l2 ++ l3
        SetupNextAction
    }

    protected def setupNext() : ForcedAction = {
        val pending = setup.%!(states.contains)

        if (pending.any) {
            val f = pending.head

            factions :+= f

            SetupFactionAction(f)
        }
        else
            InitDoneAction
    }

    protected def setupFaction(f : Faction) : Continue = {
        implicit val g : Game = this

        f match {
            case f : Knight.type =>
                states += f -> new KnightPlayer(this, f)
                Shuffle3(Treasures.all, Events.eventsFor(setup), SideQuests.questsFor(setup), ShuffledTreasuresEventsQuestsAction(f, _, _, _))

            case f : Goblins.type =>
                states += f -> new GoblinsPlayer(this, f)
                f.monsters.pile = Monsters.all
                f.secrets.pile = Secrets.all
                SetupNextAction

            case f : Dragon.type =>
                states += f -> new DragonPlayer(this, f)
                f.powers = $(FreeMove)
                DrawPowersAction(f, 3, SetupNextAction)

            case f : Cave.type =>
                states += f -> new CavePlayer(this, f)
                DrawOmensAction(f, 3, SetupNextAction)

            case f : Thief.type =>
                setupThief(f)
        }
    }

    protected def setupKnightDraws(f : Knight.type, t : $[Treasure], e : $[Event], q : $[SideQuest]) : ForcedAction = {
        implicit val g : Game = this

        f.sidequests = q
        treasures = t
        events = e
        SetupNextAction
    }

    protected def initDone() : ForcedAction =
        StartPlayerTurnAction(factions(0))
    // END_BLOCK_GAME_SETUP_DISPATCH
}
