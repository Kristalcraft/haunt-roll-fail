// FILE: vast/meta.scala
// VERSION: 0.1.0
// START_MODULE_CONTRACT
// PURPOSE: Register Vast in the shared HRF runtime and define the public factories, validation, assets, and bot bindings.
// SCOPE: Validate faction combinations, create Game instances, delegate serialization, select bots, and expose assets.
// DEPENDS: vast.Game, vast.Serialize, vast.BotKnight, vast.BotDragon, vast.BotXX, hrf.meta.MetaGame
// LINKS: M-VAST-META, M-VAST-GAME, M-VAST-SERIALIZE, M-VAST-BOT, M-VAST-UI
// ROLE: RUNTIME
// MAP_MODE: EXPORTS
// END_MODULE_CONTRACT
//
// START_MODULE_MAP
// Meta - public Vast metadata entry point used by the shared HRF runtime
// END_MODULE_MAP
//
// START_CHANGE_SUMMARY
// LAST_CHANGE: v0.1.0 - Added initial GRACE contracts, semantic blocks, and stable log markers for the first governed Vast wave.
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

import hrf.meta._
import hrf.elem._

object Meta extends MetaGame {
    val gaming = vast.gaming

    private def logMarker(scope : String, block : String, message : String) : Unit = {
        +++("[VastMeta][" + scope + "][" + block + "] " + message)
    }

    type F = Faction

    def tagF = implicitly

    val name = "vast"
    val label = "Vast: The Crystal Caverns"

    val factions = $(Knight, Goblins, Dragon, Cave, Thief).but(Thief)

    val minPlayers = 4

    override val quickFactions = factions.take(4)

    val options = $

    val quickMin = 4
    val quickMax = 4

    def randomGameName() = {
        val n = $("Dungeon", "Exploration", "Cavern", "Fate", "Grit", "Rage", "Monster").shuffle
        val c = $("for", "against", "versus", "through", "and", "of", "in", "as").shuffle
        n.head + " " + c.head + " " + n.last
    }

    // START_CONTRACT: validateFactionCombination
    // PURPOSE: Validate whether the requested faction list is supported before game creation.
    // INPUTS: { ff: $[Faction] - requested faction list }
    // OUTPUTS: { InfoResult - success or explicit validation error chain }
    // SIDE_EFFECTS: console log marker for verification
    // LINKS: M-VAST-META, V-M-VAST-META
    // END_CONTRACT: validateFactionCombination
    def validateFactionCombination(ff : $[Faction]) = {
        // START_BLOCK_VALIDATE_FACTIONS
        logMarker("validateFactions", "BLOCK_VALIDATE_FACTIONS", "requested=" + ff./(_.short).mkString(","))
        val result = InfoResult("Vast") &&
            (ff.has(Thief)).?(ErrorResult("Thief not implemented yet")) &&
            (ff.num < 4).?(ErrorResult("Select at least four factions")) &&
            (ff.num > 4).?(ErrorResult("Max four factions"))
        // END_BLOCK_VALIDATE_FACTIONS
        result
    }

    def validateFactionSeatingOptions(ff : $[Faction], options : $[O]) = None ||
        (ff.intersect(factions) != factions.intersect(ff)).?(ErrorResult("Incorrect faction order")) |
        InfoResult("Vast")

    def factionName(f : Faction) = f.name
    def factionElem(f : Faction) = f.name.styled(f)

    // override def glyph(g : G) : |[String] = g.current.?./(_.style + "-glyph")
    // override def glyph(f : F) : |[String] = |(f.style + "-glyph")
    // override def glyph(g : G, f : F) : |[String] = glyph(f).%!(_ => g.highlightFaction.has(f) && hrf.HRF.uptime() / 1000 % 2 == 1)

    // START_CONTRACT: createGame
    // PURPOSE: Build a new Vast Game instance from validated factions and options.
    // INPUTS: { factions: $[Faction] - active factions, options: $[O] - selected game options }
    // OUTPUTS: { Game - initialized Vast game object }
    // SIDE_EFFECTS: console log marker for creation attempts
    // LINKS: M-VAST-META, M-VAST-GAME
    // END_CONTRACT: createGame
    def createGame(factions : $[Faction], options : $[O]) = {
        // START_BLOCK_CREATE_GAME
        logMarker("createGame", "BLOCK_CREATE_GAME", "factions=" + factions./(_.short).mkString(",") + " options=" + options.num)
        new Game(factions, options)
        // END_BLOCK_CREATE_GAME
    }

    def getBots(f : Faction) = $("Easy")

    def getBot(f : Faction, b : String) = (f, b) match {
        case (f : Knight.type, "Easy") => new BotKnight(f)
        case (f : Dragon.type, "Easy") => new BotDragon(f)
        case (f : Faction, "Easy") => new BotXX(f)
    }

    def defaultBot(f : Faction) = "Easy"

    def writeFaction(f : Faction) = f.short
    def parseFaction(s : String) : Option[Faction] = factions.%(_.short == s).single

    def writeOption(f : O) = "n/a"
    def parseOption(s : String) = $

    def parseAction(s : String) : Action = Serialize.parseAction(s)
    def writeAction(a : Action) : String = Serialize.write(a)

    val start = StartAction(gaming.version)

    override def bodyFont = Some("dwarven-axe-bb")

    val assets =
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true, "icons")(
        ImageAsset("population"       ) ::
        ImageAsset("population-lost"  ) ::
        ImageAsset("population-fangs" ) ::
        ImageAsset("population-bones" ) ::
        ImageAsset("population-eye"   ) ::
        ImageAsset("malaise"          ) ::
        ImageAsset("malaise-fangs"    ) ::
        ImageAsset("malaise-bones"    ) ::
        ImageAsset("malaise-eye"      ) ::
        ImageAsset("malaise-strength" ) ::
        ImageAsset("strength"         ) ::

        ImageAsset("stamina"         ) ::
        ImageAsset("stamina-lost"    ) ::
        ImageAsset("stamina-used"    ) ::
        ImageAsset("stamina-discard" ) ::
        ImageAsset("stamina-poison"  ) ::

        ImageAsset("movement-knight"      ) ::
        ImageAsset("perception-knight"    ) ::
        ImageAsset("strength-knight"      ) ::

        ImageAsset("movement"      ) ::
        ImageAsset("perception"    ) ::

        ImageAsset("card-claw"     ) ::
        ImageAsset("card-flame"    ) ::
        ImageAsset("card-wing"     ) ::
        ImageAsset("free-move"     ) ::

        ImageAsset("power-claw"    ) ::
        ImageAsset("power-flame"   ) ::
        ImageAsset("power-wing"    ) ::
        ImageAsset("power-any"     ) ::

        ImageAsset("sloth"             ) ::
        ImageAsset("sloth-free"        ) ::
        ImageAsset("sloth-free-gems"   ) ::
        ImageAsset("sloth-free-stay"   ) ::
        ImageAsset("sloth-treasure"    ) ::
        ImageAsset("sloth-eaten-one"   ) ::
        ImageAsset("sloth-eaten-two"   ) ::

        ImageAsset("dragon-die"    ) ::

        ImageAsset("omen-chasm"     ) ::
        ImageAsset("omen-trail"     ) ::
        ImageAsset("omen-mushroom"  ) ::
        ImageAsset("omen-quartz"    ) ::
        ImageAsset("omen-boulder"   ) ::
        ImageAsset("omen-bat"       ) ::

        ImageAsset("omen-icon-chasm"     ) ::
        ImageAsset("omen-icon-trail"     ) ::
        ImageAsset("omen-icon-mushroom"  ) ::
        ImageAsset("omen-icon-quartz"    ) ::
        ImageAsset("omen-icon-boulder"   ) ::
        ImageAsset("omen-icon-bat"       ) ::

        ImageAsset("pattern-around"      ) ::
        ImageAsset("pattern-center"      ) ::
        ImageAsset("pattern-corners"     ) ::
        ImageAsset("pattern-cross"       ) ::
        ImageAsset("pattern-diamond"     ) ::
        ImageAsset("pattern-full-square" ) ::
        ImageAsset("pattern-saltire"     ) ::

        ImageAsset("move-deg-0"    ) ::
        ImageAsset("move-deg-90"   ) ::
        ImageAsset("move-deg-180"  ) ::
        ImageAsset("move-deg-270"  ) ::
    $) ::
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true, "figures")(
        ImageAsset("knight" ) ::
        ImageAsset("tribe-fangs" ) ::
        ImageAsset("tribe-bones" ) ::
        ImageAsset("tribe-eye" ) ::
        ImageAsset("dragon-awake" ) ::
        ImageAsset("dragon-sleeping" ) ::
        ImageAsset("chest" ) ::
        ImageAsset("chest-highlight" ) ::
        ImageAsset("event" ) ::
        ImageAsset("ambush" ) ::
        ImageAsset("crystal" ) ::
        ImageAsset("cave" ) ::

        ImageAsset("gem-claw" ) ::
        ImageAsset("gem-flame" ) ::
        ImageAsset("gem-wing" ) ::

        ImageAsset("flame-wall" ) ::
    $) ::
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true, "tiles")(
        ImageAsset("empty") ::

        ImageAsset("pending") ::

        ImageAsset("letter-a") ::
        ImageAsset("letter-b") ::
        ImageAsset("letter-c") ::
        ImageAsset("letter-d") ::
        ImageAsset("letter-e") ::
        ImageAsset("letter-f") ::
        ImageAsset("letter-g") ::
        ImageAsset("letter-h") ::
        ImageAsset("letter-i") ::
        ImageAsset("letter-j") ::
        ImageAsset("letter-k") ::
        ImageAsset("letter-l") ::
        ImageAsset("letter-m") ::
        ImageAsset("letter-n") ::
        ImageAsset("letter-o") ::
        ImageAsset("letter-p") ::
        ImageAsset("letter-q") ::
        ImageAsset("letter-r") ::
        ImageAsset("letter-s") ::
        ImageAsset("letter-t") ::
        ImageAsset("letter-u") ::
        ImageAsset("letter-v") ::
        ImageAsset("letter-w") ::
        ImageAsset("letter-x") ::
        ImageAsset("letter-y") ::
        ImageAsset("letter-z") ::

        ImageAsset("letter-aa") ::
        ImageAsset("letter-bb") ::
        ImageAsset("letter-cc") ::
        ImageAsset("letter-dd") ::
        ImageAsset("letter-ee") ::
        ImageAsset("letter-ff") ::
        ImageAsset("letter-gg") ::
        ImageAsset("letter-hh") ::
        ImageAsset("letter-ii") ::
        ImageAsset("letter-jj") ::
        ImageAsset("letter-kk") ::
        ImageAsset("letter-ll") ::
        ImageAsset("letter-mm") ::
        ImageAsset("letter-nn") ::
        ImageAsset("letter-oo") ::
        ImageAsset("letter-pp") ::
        ImageAsset("letter-qq") ::
        ImageAsset("letter-rr") ::
        ImageAsset("letter-ss") ::
        ImageAsset("letter-tt") ::
        ImageAsset("letter-uu") ::
        ImageAsset("letter-vv") ::
        ImageAsset("letter-ww") ::
        ImageAsset("letter-xx") ::
        ImageAsset("letter-yy") ::
        ImageAsset("letter-zz") ::

        ImageAsset("revealing") ::
        ImageAsset("selecting") ::

        ImageAsset("rockslide") ::

        ImageAsset("bombed") ::

        ImageAsset("hidden-fangs") ::
        ImageAsset("hidden-bones") ::
        ImageAsset("hidden-eye") ::

        ImageAsset("crystal-side-3") ::
        ImageAsset("crystal-open-3") ::
        ImageAsset("crystal-corner-3") ::
        ImageAsset("crystal-open-2") ::
        ImageAsset("crystal-corner-2") ::
        ImageAsset("crystal-side-2") ::
        ImageAsset("crystal-corner-1") ::
        ImageAsset("crystal-side-1") ::
        ImageAsset("crystal-open-1") ::
        ImageAsset("entrance") ::
        ImageAsset("pit") ::
        ImageAsset("lake") ::
        ImageAsset("mushrooms") ::
        ImageAsset("empty-side-a") ::
        ImageAsset("empty-open-c") ::
        ImageAsset("empty-open-b") ::
        ImageAsset("empty-side-9") ::
        ImageAsset("empty-side-8") ::
        ImageAsset("empty-corner-b") ::
        ImageAsset("empty-side-7") ::
        ImageAsset("empty-corner-a") ::
        ImageAsset("empty-corner-9") ::
        ImageAsset("empty-corner-8") ::
        ImageAsset("empty-tunnel-7") ::
        ImageAsset("empty-side-6") ::
        ImageAsset("empty-open-a") ::
        ImageAsset("empty-open-9") ::
        ImageAsset("empty-open-8") ::
        ImageAsset("empty-side-1") ::
        ImageAsset("empty-deadend-2") ::
        ImageAsset("empty-corner-7") ::
        ImageAsset("empty-open-7") ::
        ImageAsset("empty-corner-6") ::
        ImageAsset("empty-corner-5") ::
        ImageAsset("empty-open-6") ::
        ImageAsset("empty-side-5") ::
        ImageAsset("empty-open-5") ::
        ImageAsset("empty-tunnel-6") ::
        ImageAsset("empty-tunnel-5") ::
        ImageAsset("empty-side-4") ::
        ImageAsset("empty-tunnel-4") ::
        ImageAsset("empty-corner-4") ::
        ImageAsset("empty-corner-3") ::
        ImageAsset("empty-open-4") ::
        ImageAsset("empty-deadend-1") ::
        ImageAsset("empty-corner-2") ::
        ImageAsset("empty-open-3") ::
        ImageAsset("empty-tunnel-3") ::
        ImageAsset("empty-tunnel-2") ::
        ImageAsset("empty-corner-1") ::
        ImageAsset("empty-side-3") ::
        ImageAsset("empty-open-2") ::
        ImageAsset("empty-side-2") ::
        ImageAsset("empty-open-1") ::
        ImageAsset("empty-tunnel-1") ::
        ImageAsset("canyon") ::
        ImageAsset("magma") ::
        ImageAsset("river") ::
    $) ::
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true, "board")(
        ImageAsset("knight-board") ::
        ImageAsset("goblins-board") ::
        ImageAsset("dragon-board") ::
        ImageAsset("cave-board") ::
    $) ::
    ConditionalAssetsList((factions : $[Faction], options : $[O]) => true, "cards")(
        ImageAsset("event-light") ::
        ImageAsset("event-ambush-1") ::
        ImageAsset("event-ambush-2") ::
        ImageAsset("event-ambush-3") ::
        ImageAsset("event-ambush-4") ::
        ImageAsset("event-vantage-point") ::
        ImageAsset("event-fresh-air") ::
        ImageAsset("event-fresh-water") ::
        ImageAsset("event-cave-bread") ::
        ImageAsset("event-rats") ::
        ImageAsset("event-deep-and-dark-cave-1") ::
        ImageAsset("event-deep-and-dark-cave-2") ::
        ImageAsset("event-deep-and-dark-cave-3") ::
        ImageAsset("event-deep-and-dark-knight-1") ::
        ImageAsset("event-deep-and-dark-knight-2") ::
        ImageAsset("event-deep-and-dark-knight-3") ::

        ImageAsset("equipment-shield") ::
        ImageAsset("equipment-bow") ::
        ImageAsset("equipment-ancient-map") ::
        ImageAsset("equipment-bomb") ::

        ImageAsset("treasure-javelin") ::
        ImageAsset("treasure-elvish-sword") ::
        ImageAsset("treasure-mighty-axe") ::
        ImageAsset("treasure-potion-kit") ::
        ImageAsset("treasure-pixie-lantern") ::
        ImageAsset("treasure-enchanted-bow") ::
        ImageAsset("treasure-heroic-boots") ::

        ImageAsset("sidequest-persistent") ::
        ImageAsset("sidequest-eagle-eyed") ::
        ImageAsset("sidequest-swift") ::
        ImageAsset("sidequest-adventurous") ::
        ImageAsset("sidequest-bedecked") ::
        ImageAsset("sidequest-cunning-dragon") ::
        ImageAsset("sidequest-fearless-goblins") ::
        ImageAsset("sidequest-daring-goblins-dragon") ::
        ImageAsset("sidequest-intrepid") ::
        ImageAsset("sidequest-stalwart-ambushes") ::
        ImageAsset("sidequest-stalwart-goblins") ::

        ImageAsset("war-card-thirst") ::
        ImageAsset("war-card-spite") ::
        ImageAsset("war-card-consumption") ::
        ImageAsset("war-card-desolation") ::
        ImageAsset("war-card-waste") ::
        ImageAsset("war-card-ruin") ::
        ImageAsset("war-card-hate") ::
        ImageAsset("war-card-fear") ::
        ImageAsset("war-card-desperation") ::
        ImageAsset("war-card-pain") ::

        ImageAsset("monster-blob") ::
        ImageAsset("monster-bright-beetles") ::
        ImageAsset("monster-flame-giant") ::
        ImageAsset("monster-gnome") ::
        ImageAsset("monster-golem") ::
        ImageAsset("monster-ogre") ::
        ImageAsset("monster-pet-frog") ::
        ImageAsset("monster-troll") ::
        ImageAsset("monster-underworm") ::
        ImageAsset("monster-wisp") ::

        ImageAsset("secret-blind-fury") ::
        ImageAsset("secret-cave-in") ::
        ImageAsset("secret-fire-bomber") ::
        ImageAsset("secret-goblin-ruby") ::
        ImageAsset("secret-hex") ::
        ImageAsset("secret-hiding-spots") ::
        ImageAsset("secret-leader") ::
        ImageAsset("secret-poison") ::
        ImageAsset("secret-secret-tunnels") ::
        ImageAsset("secret-trap") ::


    $) ::
    $
}
