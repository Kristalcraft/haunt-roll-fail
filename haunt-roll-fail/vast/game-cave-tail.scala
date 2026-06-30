// FILE: vast/game-cave-tail.scala
// VERSION: 0.1.0
// START_MODULE_CONTRACT
// PURPOSE: Cave `PartialFunction` for `Game.caveTailPart3` (omens, omens main phase, Cave end-turn).
// SCOPE: Omen draw/shuffle, omens main phase, Soporific Spores, and Cave end-turn dispatch arms.
// DEPENDS: vast.Game, Continue, Action; mix into Game only (bytecode offload).
// LINKS: M-VAST-GAME
// ROLE: RUNTIME
// MAP_MODE: LOCALS
// END_MODULE_CONTRACT
//
// START_MODULE_MAP
// GameCaveTailSupport - Cave PartialFunction for caveTailPart3 orElse chain
// caveTailCaveDispatchPart3 - Cave omen and end-turn routing before Thief and board tail
// END_MODULE_MAP
//
// START_CHANGE_SUMMARY
// LAST_CHANGE: v0.1.0 - Extracted Cave tail dispatch to reduce bytecode pressure inside Game.caveTailPart3.
// END_CHANGE_SUMMARY
package vast

import hrf.colmat._
import hrf.elem._
import vast.elem._

trait GameCaveTailSupport { self : Game =>
    protected def caveTailCaveDispatchPart3(soft : Void)(implicit g : Game) : PartialFunction[Action, Continue] = {
            // CAVE
            case DrawOmensAction(f, n, then) =>
                Shuffle(OmenCards.all.diff(f.omens), OmensShuffledAction(f, n, _, then))

            case OmensShuffledAction(f, n, l, then) =>
                f.omens ++= l.take(n)

                f.log("drew", n.hl, "omen".s(n))

                then

            case ContinuePlayerTurnAction(f : Cave.type) =>
                f.hatreds = 0
                f.plunders = 0

                val n = board.inner./(p => board.list(p).use(l => l.count(Chest) + l.count(Crystal))).sum

                val o = n @@ {
                    case 0 => 1
                    case 1 => 2
                    case 2 | 3 => 3
                    case 4 | 5 | 6 => 4
                    case 7 | 8 | 9 | 10 => 5
                    case _ => 6
                }

                DrawOmensAction(f, o, CaveMainAction(f))

            case CaveMainAction(f) => (() => {
                implicit def convert(c : OmenCard, selected : Boolean) = selected.?(c.imgs).|(c.img)

                def omens(a : Omen, b : Omen, c : Omen) = a.icon ~ "/" ~ b.icon ~ "/" ~ c.icon

                XXSelectObjectsAction(f, f.omens)
                    .withGroup(f, "uses", "Omens".styled(f))
                    .withRule(_.upTo(3).all(l => l./(_.omen).use(l => l.forall(o => l.but(o).diff(o.near).diff(o.near).none))))
                    .withThens { c =>
                        val p = c.of[OmenCard]./(_.omen).sortBy(_.id)

                        val actions =
                        factions.of[Knight.type]./(e =>
                            GiantBatsKnightAction(f, e, c).as(omens(Chasm, Bat, Boulder), "Giant Bats".hh, MDash, "Move", e)
                                .!(p.num < 1)
                                .!(p.num > 1, "too many")
                                .!(p.but(Chasm).but(Bat).but(Boulder).any, "composition")
                                .!(e.assigned.has(Shield), "shield")
                        ) ++
                        factions.of[Goblins.type]./~(e => e.tribes.%(_.hidden.not)./(t =>
                            GiantBatsGoblinsAction(f, e, t.tribe, c).as(omens(Chasm, Bat, Boulder), "Giant Bats".hh, MDash, "Move", t.tribe)
                                .!(p.num < 1)
                                .!(p.num > 1, "too many")
                                .!(p.but(Chasm).but(Bat).but(Boulder).any, "composition")
                        )) ++
                        $(
                            GiantBatsChestMainAction(f, c).as(omens(Chasm, Bat, Boulder), "Giant Bats".hh, MDash, "Move", Chest)
                                .!(p.num < 1)
                                .!(p.num > 1, "too many")
                                .!(p.but(Chasm).but(Bat).but(Boulder).any, "composition")
                        ) ++
                        $(
                            RockslideMainAction(f, c).as(2.hl, "X", omens(Bat, Boulder, Quartz), "Rockslide".hh, MDash, "Make", "Wall".styled(Cave))
                                .!(p.num < 2)
                                .!(p.num > 2, "too many")
                                .!(p.but(Bat).but(Boulder).but(Quartz).any, "composition")
                        ) ++
                        ${
                            val n = 1 + min(2, f.plunders)
                            PlaceTreasureMainAction(f, c).as(n.hl ~ (n < 3).?("+".txt), "X", omens(Boulder, Quartz, Mushroom), "Past Plunders".hh, MDash, "Place", Chest)
                                .!(p.num < n)
                                .!(p.num > n, "too many")
                                .!(p.but(Boulder).but(Quartz).but(Mushroom).any, "composition")
                        } ++
                        factions.but(f)./(e =>
                            SoporificSporesMainAction(f, e, c).as(3.hl, "X", omens(Quartz, Mushroom, Trail), "Soporific Spores".hh, MDash, "Hurt", e)
                                .!(p.num < 3)
                                .!(p.num > 3, "too many")
                                .!(p.but(Quartz).but(Mushroom).but(Trail).any, "composition")
                        ) ++
                        collapse.not.${
                            val n = 1 + min(2, f.hatreds)
                            PlaceTileMainAction(f, c).as(n.hl ~ (n < 3).?("+".txt), "X", omens(Mushroom, Trail, Chasm), "Hatred".hh, MDash, "Place Tile".hl)
                                .!(p.num < n)
                                .!(p.num > n, "too many")
                                .!(p.but(Mushroom).but(Trail).but(Chasm).any, "composition")
                        } ++
                        collapse.${
                            val n = 1 + min(2, f.hatreds)
                            RemoveTileMainAction(f, c).as(n.hl ~ (n < 3).?("+".txt), "X", omens(Mushroom, Trail, Chasm), "Hatred".hh, MDash, "Remove Tile".hl)
                                .!(p.num < n)
                                .!(p.num > n, "too many")
                                .!(p.but(Mushroom).but(Trail).but(Chasm).any, "composition")
                        } ++
                        $(
                            CrystalCurseRotateTileMainAction(f, c).as(omens(Trail, Chasm, Bat), "Crystal Curse".hh, MDash, "Rotate Tile".hl)
                                .!(p.num < 1)
                                .!(p.num > 1, "too many")
                                .!(p.but(Trail).but(Chasm).but(Bat).any, "composition")
                        ) ++
                        $(
                            CrystalCurseEventTokenMainAction(f, c).as(omens(Trail, Chasm, Bat), "Crystal Curse".hh, MDash, "Place Event Token".hl)
                                .!(p.num < 1)
                                .!(p.num > 1, "too many")
                                .!(p.but(Trail).but(Chasm).but(Bat).any, "composition")
                        ) ++
                        $(
                            CrystalCurseRecycleEventsMainAction(f, c).as(omens(Trail, Chasm, Bat), "Crystal Curse".hh, MDash, "Cycle Events".hl)
                                .!(p.num < 1)
                                .!(p.num > 1, "too many")
                                .!(p.but(Trail).but(Chasm).but(Bat).any, "composition")
                        ) ++
                        $

                        actions./~{
                            case UnavailableReasonAction(action, "composition") => None
                            case UnavailableReasonAction(action, "too many") => None
                            case action => Some(action)
                        }
                    }
                    .withExtra($(NoOmen, CaveDoneAction(f).as("End Turn")("" ~ Break)))
            })()

            case CaveCancelAction(f) =>
                board.writeAll(f, $)

                CaveMainAction(f)

            case GiantBatsKnightAction(f, e, c) =>
                f.omens = f.omens.diff(c)

                f.log("used", c.comma, "to summon", "Giant Bats".styled(f))

                ForceMoveMainAction(f, e, 3, CaveMainAction(f))

            case GiantBatsGoblinsAction(f, e, t, c) =>
                f.omens = f.omens.diff(c)

                f.log("used", c.comma, "to summon", "Giant Bats".styled(f))

                ForceMoveMainAction(f, t, 3, CaveMainAction(f))

            case GiantBatsChestMainAction(f, c) =>
                var found : $[Relative] = $

                board.outer.foreach { p =>
                    if (board.list(p).has(Chest))
                        found :+= p
                }

                val l = found

                board.writeAll(f, l)

                Ask(f).each(l)(p => GiantBatsChestAction(f, TokenAt(Chest, p), c).as("Move", Chest, "at", board.read(f, p).|("?").hl)(Image("chest", styles.illustration))).add(CaveCancelAction(f).as("Cancel"))

            case GiantBatsChestAction(f, t, c) =>
                board.writeAll(f, $)

                f.omens = f.omens.diff(c)

                f.log("used", c.comma, "to summon", "Giant Bats".styled(f))

                ForceMoveMainAction(f, t, 3, CaveMainAction(f))

            case RockslideMainAction(f, c) =>
                var found : $[(Relative, $[Bearing])] = $

                board.outer.foreach { p =>
                    if (board.get(p) != Emptiness) {
                        val dirs = $(East, South).%(dir => board.get(p.add(dir)) != Emptiness && board.wall(p, dir).not)
                        found :+= (p, dirs)
                    }
                }

                val l = found.lefts

                board.writeAll(f, l)

                Ask(f).some(found) { case (p, dirs) => dirs./{ dir =>
                    RockslideAction(f, p, dir, c).as("Place", "Rockslide".styled(f), "between", board.read(f, p).|("?").hl, "and", board.read(f, p.add(dir)).|("?").hl)(Image("rockslide", styles.illustration))
                }}.add(CaveCancelAction(f).as("Cancel"))

            case RockslideAction(f, p, dir, c) if board.rockslides.num >= 3 =>
                Ask(f).each(board.rockslides) { case (op, odir) =>
                    RockslideRemoveAction(f, op, odir, RockslideAction(f, p, dir, c)).as("Remove", "Rockslide".styled(f), "between", board.read(f, op).|("?").hl, "and", board.read(f, op.add(odir)).|("?").hl)("To place", "Rockslide".styled(f), "between", board.read(f, p).|("?").hl, "and", board.read(f, p.add(dir)).|("?").hl)
                }.add(CaveCancelAction(f).as("Cancel"))

            case RockslideRemoveAction(f, p, dir, then) =>
                board.rockslides :-= (p, dir)

                f.log("removed a", "Rockslide".styled(f))

                then

            case RockslideAction(f, p, dir, c) =>
                board.writeAll(f, $)

                f.omens = f.omens.diff(c)

                f.log("used", c.comma, "to cause", "Rockslide".styled(f))

                board.rockslides :+= (p, dir)

                f.log("placed a", "Rockslide".styled(f))

                CaveMainAction(f)

            case PlaceTreasureMainAction(f, c) =>
                f.omens = f.omens.diff(c)

                f.log("used", c.comma, "to remember", "Past Plunder".styled(f))

                f.plunders += 1

                PlaceTreasureAction(f, CaveMainAction(f))

            case PlaceTileMainAction(f, c) =>
                f.omens = f.omens.diff(c)

                f.log("used", c.comma, "for", "Hatred".styled(f))

                f.hatreds += 1

                PlaceTileAction(f, CaveCancelAction(f))

            case RemoveTileMainAction(f, c) =>
                f.omens = f.omens.diff(c)

                f.log("used", c.comma, "for", "Hatred".styled(f))

                f.hatreds += 1

                RemoveTileAction(f, $, CaveCancelAction(f))

            case SoporificSporesMainAction(f, e : Knight.type, c) =>
                f.omens = f.omens.diff(c)

                f.log("used", c.comma, "to send", "Soporific Spores".styled(f))

                f.log("sent", "Soporofic Spores".styled(Cave), "on", e)

                e.score(-5)

                CaveMainAction(f)

            case SoporificSporesMainAction(f, e : Goblins.type, c) =>
                f.omens = f.omens.diff(c)

                f.log("used", c.comma, "to send", "Soporific Spores".styled(f))

                f.log("sent", "Soporofic Spores".styled(Cave), "on", e)

                e.adjustRage(1)

                Ask(e).each(e.tribes)(t => ReducePopulationAction(e, t.tribe, t.population - 1, CaveMainAction(f)).as("Reduce", t.tribe, "population to", 1.hl)("Soporofic Spores".styled(Cave)).!(t.population <= 1)).bailw(CaveMainAction(f)) {
                    e.log("could not reduce population")
                }

            case SoporificSporesMainAction(f, e : Dragon.type, c) =>
                f.omens = f.omens.diff(c)

                f.log("used", c.comma, "to send", "Soporific Spores".styled(f))

                f.log("sent", "Soporofic Spores".styled(Cave), "on", e)

                Ask(e).each(e.tracks)(t => ReturnSlothAction(e, t.track, CaveMainAction(f)).as("Return", "Sloth".styled(styles.sloth), "to", t.track)("Soporofic Spores".styled(Cave)).!(t.value >= t.track.max)).bailw(CaveMainAction(f)) {
                    e.log("could not return", "Sloth".styled(styles.sloth))
                }

            case SoporificSporesMainAction(f, e : Thief.type, c) =>
                f.omens = f.omens.diff(c)

                f.log("used", c.comma, "to send", "Soporific Spores".styled(f))

                f.log("sent", "Soporofic Spores".styled(Cave), "on", e)

                if (e.upgrades.any) {
                    val removed = e.upgrades.shuffle(0)
                    e.upgrades :-= removed
                    e.log("lost upgrade", removed, "to Soporific Spores")
                }
                else
                    e.log("had no upgrades to lose")

                CaveMainAction(f)

            case ReturnSlothAction(f, t, then) =>
                f.tracks.%(_.track == t).foreach { _.value += 1 }

                f.log("returned", "Sloth".styled(styles.sloth), "to", t)

                f.wakefulness -= 1

                then

            case CrystalCurseRotateTileMainAction(f, c) =>
                val l = board.inner.%(p => board.get(p).as[Explored].?(t => t.walls.any || board.bombed.forany((bp, bdir) => bp == p || bp.add(bdir) == p) || board.rockslides.forany((bp, bdir) => bp == p || bp.add(bdir) == p)))

                board.writeAll(f, l)

                Ask(f).each(l)(p => RotateTileMainAction(f, p, c).as("Rotate", board.read(f, p).|("?").hl)("Rotate Tile")).add(CaveCancelAction(f).as("Cancel"))

            case RotateTileMainAction(f, p, c) =>
                val t = board.get(p).as[Explored].get

                Ask(f).each(Bearings.wnes)(dir => RotateTileAction(f, Tile(t.walls.rotate(t.bearing.reverse), t.img), p, dir, c)).add(CaveCancelAction(f).as("Cancel"))

            case RotateTileAction(f, t, p, dir, c) =>
                board.writeAll(f, $)

                f.omens = f.omens.diff(c)

                f.log("used", c.comma, "to", "Crystal Curse".styled(f))

                val t = board.get(p).as[Explored].get

                board.set(p, t.copy(bearing = dir, walls = t.walls.rotate(t.bearing).rotate(t.bearing).rotate(t.bearing).rotate(dir)))

                board.bombed = board.bombed.%!((bp, bdir) => bp == p || bp.add(bdir) == p)
                board.rockslides = board.rockslides.%!((bp, bdir) => bp == p || bp.add(bdir) == p)

                f.log("rotated tile", dir)

                ShowOpenEdgesAction(f, CaveMainAction(f))

            case CrystalCurseEventTokenMainAction(f, c) =>
                val l = board.inner.%(p => board.get(p).as[Explored].?(_.original.exists(_.tokens.has(Event)))).%(p => board.list(p).has(Event).not)

                board.writeAll(f, l)

                Ask(f).each(l)(p => EventTokenAction(f, p, c).as("Place at", board.read(f, p).|("?").hl)(Image("event", styles.illustration))).add(CaveCancelAction(f).as("Cancel"))

            case EventTokenAction(f, p, c) =>
                board.writeAll(f, $)

                f.omens = f.omens.diff(c)

                f.log("used", c.comma, "to", "Crystal Curse".styled(f))

                board.place(p, Event)

                f.log("placed", Event)

                CaveMainAction(f)

            case CrystalCurseRecycleEventsMainAction(f, c) =>
                f.omens = f.omens.diff(c)

                f.log("used", c.comma, "to", "Crystal Curse".styled(f))

                events = events.drop(3) ++ events.take(3)

                f.log("recycled", "Events".hl)

                CaveMainAction(f)


            // CAVE END TURN
            case CaveDoneAction(f) if collapse.not =>
                PlaceTileAction(f, PlaceTreasureAction(f, EndPlayerTurnAction(f)))

            case CaveDoneAction(f) if collapse =>
                RemoveTileAction(f, $, RemoveTileAction(f, $, RemoveTileAction(f, $, PlaceTreasureAction(f, EndPlayerTurnAction(f)))))

            case PlaceTileAction(f, then) if tiles.none =>
                log("No more tiles")

                then

            case PlaceTileAction(f, then) =>
                var found : $[Relative] = $

                board.outer.foreach { p =>
                    if (board.get(p) == Emptiness)
                        if (Bearings.all.exists(d => board.get(p.add(d)) != Emptiness))
                            found :+= p
                }

                val l = found

                board.writeAll(f, l)

                implicit def convert(h : HiddenTile) = {
                    (Image(h.tile.name).apply(styles.tile)(styles.abs) ~ h.tokens./(t => Image(t.toString.toLowerCase, $(styles.tile, styles.abs))) ~ Image("empty", $(styles.tile))).spn ~ Image("hidden-" + h.tribe.name).apply(styles.tile)
                }

                YYSelectObjectsAction(f, tiles.take(3))
                    .withRule(t => current != f || t.tokens.has(Crystal) || tiles.take(3).exists(_.tokens.has(Crystal)).not)
                    .withGroup(f, "places a tile")
                    .withThens(t => l./(p => PlaceHiddenTileAction(f, p, t, then).as("Place" ~ board.read(f, p)./(" at " ~ _.hl))))

            case RemoveTileAction(f, except, then) =>
                var l = board.inner.%(p => board.get(p) != Emptiness)

                val lk = l.diff(except).%(p => board.list(p).has(Entrance).not)./(p => p -> Bearings.wnes./(p.add).%(l.has).num)

                if (lk.any) {
                    val n = lk.rights.min

                    val r = lk.%((p : Relative, k : Int) => k == n).lefts

                    board.writeAll(f, r)

                    Ask(f).each(r)(p => CollapseTileAction(f, p, ReconnectMapAction(f, true, then), RemoveTileAction(f, p +: except, then)).as("Tile", board.read(f, p))("Collapse")).needOk
                }
                else {
                    f.log("could not remove any tile")

                    then
                }

            case PlaceTreasureAction(f, then) =>
                val others = factions.but(f)./~(_.positions)

                var found : $[Relative] = $

                board.inner.foreach { p =>
                    if (others.has(p).not)
                        if (board.get(p).is[HiddenTile])
                            if (board.list(p).has(Chest).not)
                                found :+= p
                }

                val l = found

                board.writeAll(f, l)

                Ask(f).each(l)(PlaceTreasureTileAction(f, _, then)).needOk.bailw(then) {
                    f.log("could not place", Chest)
                }

            case PlaceTreasureTileAction(f, p, then) =>
                board.writeAll(f, $)

                board.place(p, Chest)

                f.log("placed a", Chest)

                then

    }

}

