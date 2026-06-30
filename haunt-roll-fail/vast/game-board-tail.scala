// FILE: vast/game-board-tail.scala
// VERSION: 0.1.0
// START_MODULE_CONTRACT
// PURPOSE: Board / collapse / reconnect / fillers / generic turn tail `PartialFunction` for `Game.caveTailPart3`.
// SCOPE: Tile collapse, reconnect, map fillers, EndPlayerTurn rotation, and generic turn-tail dispatch arms.
// DEPENDS: vast.Game, Continue, Action; mix into Game only (bytecode offload).
// LINKS: M-VAST-GAME
// ROLE: RUNTIME
// MAP_MODE: LOCALS
// END_MODULE_CONTRACT
//
// START_MODULE_MAP
// GameBoardTailSupport - board/collapse/reconnect PartialFunction for caveTailPart3 orElse chain
// boardTailDispatchPart3 - generic turn tail and EndPlayerTurn faction rotation
// END_MODULE_MAP
//
// START_CHANGE_SUMMARY
// LAST_CHANGE: v0.1.0 - Extracted board tail dispatch to reduce bytecode pressure inside Game.caveTailPart3.
// END_CHANGE_SUMMARY
package vast

import hrf.colmat._
import hrf.elem._
import vast.elem._

trait GameBoardTailSupport { self : Game =>
    protected def boardTailDispatchPart3(soft : Void)(implicit g : Game) : PartialFunction[Action, Continue] = {
            // BOARD
            case CollapseTileAction(f, p, then, fail) if board.list(p).has(Entrance) =>
                f.log("could not collapse", Entrance)

                fail

            case CollapseTileAction(f, p, then, fail) if setup.of[Knight.type].exists(_.position == p) =>
                implicit val ask = builder

                val t = setup.of[Knight.type].%(_.position == p).first

                val others = factions.but(t)./~(_.positions)

                Bearings.wnes.foreach { dir =>
                    val dest = p.add(dir)

                    + ForceMoveStepAction(f, t, dir, 1, 1, $(dest), CollapseTileAction(f, p, then, fail))
                        .!(t.assigned.has(Shield))
                        .!(board.get(dest).is[Emptiness.type])
                        .!(board.get(dest).is[HiddenTile])
                        .!(board.wall(p, dir))
                        .!(others.has(dest))
                }

                ask(f).needOk.bailw(fail) {
                    f.log("could not collapse a tile and move away", t)
                }

            case CollapseTileAction(f, p, then, fail) if setup.of[Dragon.type].exists(_.position.has(p)) =>
                implicit val ask = builder

                val t = setup.of[Dragon.type].%(_.position.has(p)).first

                val others = factions.but(t)./~(_.positions)

                Bearings.wnes.foreach { dir =>
                    val dest = p.add(dir)

                    + ForceMoveStepAction(f, t, dir, 1, 1, $(dest), CollapseTileAction(f, p, then, fail))
                        .!(board.get(dest).is[Emptiness.type])
                        .!(t.underground.not && board.get(dest).is[HiddenTile])
                        .!(t.underground.not && board.wall(p, dir))
                        .!(others.has(dest))
                }

                ask(f).needOk.bailw(fail) {
                    f.log("could not collapse a tile and move away", t)
                }

            case CollapseTileAction(f, p, then, fail) if setup.of[Goblins.type].exists(_.positions.has(p)) =>
                implicit val ask = builder

                val t = setup.of[Goblins.type].%(_.positions.has(p)).first

                val village = t.tribes.%(_.position.has(p)).first

                val others = factions.but(t)./~(_.positions)

                Bearings.wnes.foreach { dir =>
                    val dest = p.add(dir)

                    + ForceMoveStepAction(f, village.tribe, dir, 1, 1, $(dest), CollapseTileAction(f, p, then, fail))
                        .!(board.get(dest).is[Emptiness.type])
                        .!(board.wall(p, dir))
                        .!(others.has(dest))
                }

                ask(f).needOk.bailw(fail) {
                    f.log("could not collapse a tile and move away", t)
                }

            case CollapseTileAction(f, p, then, fail) =>
                val t = board.get(p)

                board.set(p, Emptiness)

                board.bombed = board.bombed.%!((bp, bdir) => bp == p || bp.add(bdir) == p)
                board.rockslides = board.rockslides.%!((bp, bdir) => bp == p || bp.add(bdir) == p)

                f.log("collapsed a tile")

                val dark = t match {
                    case Removing(t : HiddenTile) => |(t)
                    case Removing(t : Explored) => t.original
                    case _ => None
                }

                log(">>>", t.toString)

                log("Hiiden Tile", dark.toString)

                dark.foreach { t =>
                    if (t.tokens.has(Crystal)) {
                        log(Crystal, "tile was removed")

                        shrunk += 1
                    }
                    else
                        collapsed :+= t
                }

                board.list(p).foreach { o =>
                    f.log("removed", o)

                    board.remove(p, o)

                    o @@ {
                        case DragonGem(f, p) =>
                            f.gems :-= p
                        case _ =>
                    }
                }

                if (shrunk >= 5) {
                    board.squeeze(factions./~(_.positions))

                    Cave.log("collapsed", 5.hl, Crystal, "tiles")

                    Milestone(GameOverAction($(Cave)))
                }
                else
                    then

            case ReconnectMapAction(f, mark, then) if collapsed.any && collapse =>
                collapsed = $

                ReconnectMapAction(f, mark, then)

            case ReconnectMapAction(f, mark, then) if collapsed.any =>
                Shuffle(collapsed, AddCollapsedTilesAction(_, ReconnectMapAction(f, mark, then)))

            case AddCollapsedTilesAction(l, then) =>
                tiles ++= l

                collapsed = $

                then

            case ReconnectMapAction(f, mark, then) =>
                factions.foreach { f =>
                    if (states.contains(f)) {
                        f.positions.foreach { p =>
                            if (board.valid(p).not) {
                                println("out of map " + f + " " + p)
                                throw new Error("out of map " + f + " " + p)
                            }
                        }
                    }
                }

                board.squeeze(factions./~(_.positions))

                val map = Array.fill(board.width, board.height)(0)
                val regions = collection.mutable.Map[Int, Int](0 -> 0)

                1.until(board.width - 1).foreach { x =>
                    1.until(board.height - 1).foreach { y =>
                        if (board.cells(x)(y) != Emptiness) {
                            map(x)(y) = (map(x - 1)(y), map(x)(y - 1)) match {
                                case (0, 0) =>
                                    val nv = regions.size
                                    regions(nv) = nv
                                    nv
                                case (vx, 0) => regions(vx)
                                case (0, vy) => regions(vy)
                                case (vx, vy) if regions(vx) == regions(vy) => regions(vx)
                                case (vx, vy) =>
                                    regions.keys.filter(k => regions(k) == regions(vy)).foreach(k => regions(k) = regions(vx))
                                    regions(vx)
                            }
                        }
                    }
                }

                val groups = regions.values.$.%(_ > 0).distinct

                if (groups.num <= 1) {
                    board.letters -= f

                    ShowOpenEdgesAction(f, then)
                }
                else {
                    if (mark)
                        log("Reconnecting map")

                    if (mark)
                        board.letters += f -> Array.fill(board.width, board.height)(None : |[String])

                    val ll : $[$[Absolute]] = regions.values.$.%(_ > 0).distinct./{ n =>
                        1.until(board.width - 1)./~{ x =>
                            1.until(board.height - 1)./~{ y =>
                                (regions(map(x)(y)) == n).?(Absolute(x, y))
                            }
                        }
                    }

                    val ee : $[$[(Bearing, Int)]] = ll./{ l =>
                        val e = l./~(p => $(Absolute(p.x - 1, p.y), Absolute(p.x + 1, p.y), Absolute(p.x, p.y - 1), Absolute(p.x, p.y + 1))).distinct
                        Bearings.wnes./~{ dir =>
                            ll.but(l)./~{ o =>
                                o./~(p => e.%(q => (p.x - q.x).sign == dir.dx && (p.y - q.y).sign == dir.dy)./(q => dir -> ((p.x - q.x).abs + (p.y - q.y).abs)))
                            }
                        }
                    }

                    val bb : $[Box] = ll./{ l =>
                        val xx = l./(_.x)
                        val yy = l./(_.y)
                        Box(xx.min, yy.min, xx.max, yy.max)
                    }

                    if (mark)
                        ll.zip(board.lettering)./ { (l, s) =>
                            l.foreach { p =>
                                board.letters(f)(p.x)(p.y) = |(s)
                            }
                        }

                    implicit val ask = builder

                    ll.lazyZip(ee).foreach { (l, b) =>
                        Bearings.wnes.foreach { dir =>
                            val n = b.%((d, _) => d == dir)./((_, n) => n).minOr(0)

                            + MoveMapSegmentAction(f, l, dir, n, then).!(n == 0)
                        }
                    }

                    ask(f).needOk.bailw(then) {
                        f.log("could not reconnect map")
                    }
                }

            case MoveMapSegmentAction(f, l, dir, n, then) =>
                f.log("moved map segment", dir)

                var ll = l./(p => Relative(p.x - board.center.x, p.y - board.center.y))

                0.until(n).foreach { _ =>
                    board.move(ll./(p => Absolute(p.x + board.center.x, p.y + board.center.y)), dir.dx, dir.dy)
                    board.expand()

                    factions.foreach {
                        case f : Knight.type =>
                            if (ll.has(f.position))
                                f.position = f.position.add(dir)

                        case f : Goblins.type =>
                            f.tribes.foreach { t =>
                                if (t.position.exists(ll.has))
                                    t.position = t.position./(_.add(dir))
                            }

                        case f : Dragon.type =>
                            if (f.position.exists(ll.has))
                                f.position = f.position./(_.add(dir))

                        case _ =>
                    }

                    if (ll.has(board.entrance))
                        board.entrance = board.entrance.add(dir)

                    ll = ll./(_.add(dir))
                }

                board.squeeze(factions./~(_.positions))

                ReconnectMapAction(f, false, then)

            case ShowOpenEdgesAction(f, then) if collapse =>
                then

            case ShowOpenEdgesAction(f, then) if tiles.none =>
                log("No more tiles")

                then

            case ShowOpenEdgesAction(f, then) if factions.has(Cave) && f != Cave =>
                ShowOpenEdgesAction(Cave, then)

            case ShowOpenEdgesAction(f, then) =>
                var found : $[Relative] = $

                board.inner.foreach { p =>
                    if (board.get(p).is[Explored]) {
                        Bearings.all.foreach { d =>
                            val e = p.add(d)
                            if (board.get(e) == Emptiness && board.wall(p, d).not) {
                                found :+= e
                                board.set(e, Pending)
                            }
                        }
                    }
                }

                found = found.distinct

                if (found.none)
                    then
                else {
                    board.expand()

                    if (found.num > 1) {
                        found = found.sortBy(p => p.x * board.height + p.y)

                        board.writeAll(f, found)
                    }

                    FillOpenEdgesAction(f, found, then)
                }

            case FillOpenEdgesAction(f : Cave.type, l, then) if l.none && "TERRAIN" == "TILES" =>
                var found : $[Relative] = $

                board.outer.foreach { p =>
                    if (board.get(p) == Emptiness)
                        if (Bearings.all.exists(d => board.get(p.add(d)) != Emptiness))
                            found :+= p
                }

                val l = found

                board.writeAll(f, l)

                Ask(f)
                    .each(l)(p => PlaceMagmaAction(f, p, then).as(board.read(f, p).|("?").hl)("Magma"))
                    .each(l)(p => PlaceRiverAction(f, p, then).as(board.read(f, p).|("?").hl)("River"))
                    .skip(PlaceNothingAction(f, then))

            case PlaceNothingAction(f, then) =>
                board.writeAll(f, $)

                then

            case PlaceMagmaAction(f, p, then) =>
                board.writeAll(f, $)

                board.set(p, Magma("magma", North))

                f.log("placed", "Magma".styled(styles.hit))

                then

            case PlaceRiverAction(f, p, then) =>
                board.writeAll(f, $)

                board.set(p, River("river", West))
                board.expand()

                f.log("placed", "River".styled(Goblins))

                then

            case FillOpenEdgesAction(f, l, then) if l.none =>
                then

            case FillOpenEdgesAction(f, l, then) if tiles.none =>
                board.outer.foreach { p =>
                    if (board.get(p) == Pending)
                        board.set(p, Emptiness)
                }

                then

            case FillOpenEdgesAction(f, l, then) if "DEBUG" == "REMOVE" =>
                PlaceHiddenTileAction(f, l(0), tiles(0), FillOpenEdgesAction(f, l.but(l(0)), then))

            case FillOpenEdgesAction(f : Cave.type, l, then) =>
                implicit def convert(h : HiddenTile) = {
                    (Image(h.tile.name).apply(styles.tile)(styles.abs) ~ h.tokens./(t => Image(t.toString.toLowerCase, $(styles.tile, styles.abs))) ~ Image("empty", $(styles.tile))).spn ~ Image("hidden-" + h.tribe.name).apply(styles.tile)
                }

                YYSelectObjectsAction(f, tiles.take(3))
                    .withRule(t => current != f || t.tokens.has(Crystal) || tiles.take(3).exists(_.tokens.has(Crystal)).not)
                    .withGroup(f, "places a tile")
                    .withThens(t => l./(p => PlaceHiddenTileAction(f, p, t, FillOpenEdgesAction(f, l.but(p), then)).as("Place" ~ board.read(f, p)./(" at " ~ _.hl))))

            case FillOpenEdgesAction(f, l, then) =>
                implicit def convert(h : HiddenTile) = {
                    Image("hidden-" + h.tribe.name).apply(styles.tile)
                }

                YYSelectObjectsAction(f, tiles.take(1))
                    .withGroup(f, "places a tile")
                    .withThens(t => l./(p => PlaceHiddenTileAction(f, p, t, FillOpenEdgesAction(f, l.but(p), then)).as("Place" ~ board.read(f, p)./(" at " ~ _.hl))))

            case PlaceHiddenTileAction(f, p, t, then) =>
                tiles :-= t

                board.set(p, t)

                board.write(f, p, "")

                board.expand()

                f.log("placed a hidden tile")

                then

            case ContinuePlayerTurnAction(f) =>
                implicit val ask = builder

                + EndPlayerTurnAction(f).as("End Turn")

                ask(f).needOk

            case EndPlayerTurnAction(f) =>
                factions = factions.drop(1) ++ factions.take(1)

                StartPlayerTurnAction(factions(0))

            case GameOverAction(winners) =>
                isOver = true

                winners.foreach(f => f.log("won"))

                GameOver(winners, "Game Over", winners./~(f => $(GameOverWonAction(null, f))))

            // HELPERS
            case a : SelfPerform =>
                a.perform(soft)(this)

    }

}

