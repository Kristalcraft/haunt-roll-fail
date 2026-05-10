// FILE: vast/game-dragon.scala
// VERSION: 0.1.0
// START_MODULE_CONTRACT
// PURPOSE: Isolate Dragon faction `PartialFunction` dispatch used from `Game.performInternalPart3`.
// DEPENDS: vast.Game, Continue, Action; mix into Game only (bytecode offload from monolithic matcher).
// LINKS: M-VAST-GAME
// ROLE: RUNTIME
// END_MODULE_CONTRACT
package vast

import hrf.colmat._
import hrf.elem._
import vast.elem._

trait GameDragonSupport { self : Game =>
    protected def dragonDispatchPart3(soft : Void)(implicit g : Game) : PartialFunction[Action, Continue] = {
            // DRAGON
            case ContinuePlayerTurnAction(f : Dragon.type) =>
                if (f.position.none) {
                    f.position = Some(factions.has(Knight).?(Knight.position).|(Relative(0, 0)))

                    f.log("was heard")
                }
                else {
                    f.powers = $(FreeMove) ++ f.gems./(GemCard) ++ f.powers

                    f.path = $

                    f.events = 0

                    board.inner.foreach { p =>
                        board.remove(p, FlameWall(f))
                    }
                }

                DragonTurnAction(f)

            case DragonCheckAction(f, then) => (() => {
                if (f.treasures > 0 && f.greed.available) {
                    f.treasures -= 1

                    f.greed.reduce()

                    f.wakefulness += 1

                    f.log("satisfied", "Greed".hl)
                }

                if (f.eaten >= 2 && f.hunger.available) {
                    f.eaten -= 2

                    f.hunger.reduce()

                    f.wakefulness += 1

                    f.log("satisfied", "Hunger".hl)
                }

                if (f.events >= 1 && f.prideE.available) {
                    f.events -= 1

                    f.prideE.reduce()

                    f.wakefulness += 1

                    f.log("satisfied", "Pride".hl)
                }

                if (f.awake.not && f.wakefulness >= 11) {
                    f.awake = true

                    f.log("has awaken")
                }
            })()

                if (f.awake && f.underground.not && f.position.?(p => board.list(p).has(Entrance))) {
                    f.log("escaped the", Cave)

                    Milestone(GameOverAction($(f)))
                }
                else
                    then

            case DragonTurnAction(f) =>
                DragonCheckAction(f, DragonMainAction(f))

            case DragonMainAction(f) =>
                implicit def convert(c : DragonCard, selected : Boolean) = selected.?(c.imgs).|(c.img)

                XXSelectObjectsAction(f, f.powers)
                    .withGroup(f, "uses", "Powers".styled(f))
                    .withRule(_.upTo(4).all(l => l.has(FreeMove).not || l.num == 1))
                    .withThens { c =>
                        val p = (c.of[GemCard]./(_.power) ++ c.of[PowerCard]./(_.power)).sortBy(_.id)

                        $(DragonMoveMainAction(f, false, $(FreeMove)).as("Free Move".hh).!(c != $(FreeMove))) ++
                        $(DragonAttackMainAction(f, c).as(dt.Claw, "Attack", dt.DragonDie).!(p != $(Claw))) ++
                        $(DragonRevealMainAction(f, c).as(dt.Flame, "Reveal", dt.DragonDie).!(p != $(Flame)).!(board.pattern(f.position.get, FullSquare).exists(board.get(_).is[HiddenTile]).not, "nothing to reveal")) ++
                        $(DragonMoveMainAction(f, true, c).as(dt.Wing, "Move").!(p != $(Wing))) ++
                        $(DragonHissMainAction(f, c).as(dt.Claw ~ dt.Claw, "Hiss").!(p != $(Claw, Claw))) ++
                        $(DragonSlitherMainAction(f, c).as(dt.Claw ~ dt.Flame, "Slither").!(p != $(Claw, Flame))) ++
                        $(DragonSwatMainAction(f, c).as(dt.Claw ~ dt.Wing, "Swat").!(p != $(Claw, Wing))) ++
                        $(DragonScorchMainAction(f, c).as(dt.Flame ~ dt.Flame, "Scorch").!(p != $(Flame, Flame)).!(board.pattern(f.position.get, Around).exists(board.get(_).is[HiddenTile]).not, "nothing to reveal")) ++
                        $(DragonBurnMainAction(f, c).as(dt.Flame ~ dt.Wing, "Burn").!(p != $(Flame, Wing)).!(board.inner.exists(board.get(_).is[HiddenTile]).not)) ++
                        $(DragonSlapMainAction(f, c).as(dt.Wing ~ dt.Wing, "Slap").!(p != $(Wing, Wing))) ++
                        $(DragonShriekMainAction(f, c).as(dt.PowerAny ~ dt.PowerAny, "Shriek").!(p.num != 2).!(f.shriek)) ++
                        $(DragonScratchMainAction(f, c).as(dt.Claw ~ dt.Claw ~ dt.Claw, "Scratch").!(p != $(Claw, Claw, Claw))) ++
                        $(DragonFlameWallMainAction(f, c).as(dt.Flame ~ dt.Flame ~ dt.Flame, "Flame Wall").!(p != $(Flame, Flame, Flame))) ++
                        $(DragonSmashMainAction(f, c).as(dt.PowerAny ~ dt.PowerAny ~ dt.PowerAny, "Smash").!(p.num != 3).!(board.list(f.position.get).has(Crystal).not)) ++
                        $(DragonWrathMainAction(f, c).as(dt.PowerAny ~ dt.PowerAny ~ dt.PowerAny ~ dt.PowerAny, "Wrath").!(p.num != 4)) ++
                        $
                    }
                    .withExtra($(DragonDoneForfeitAction(f).as("End Turn")("" ~ Break)))

            case DragonMoveMainAction(f, force, c) =>
                implicit val ask = builder

                Bearings.wnes.foreach { dir =>
                    val wall = board.wall(f.position.get, dir)

                    val dest = f.position.get.add(dir)

                    val cell = board.get(dest)

                    + DragonMoveAction(f, dir, c, (f.underground.not && wall).?(DragonTurnAction(f)).|(DragonMoveContinueAction(f, dir)))
                        .!(f.underground.not && wall && force.not, "wall")
                        .!(cell.is[Emptiness.type], "can't move to open space")
                        .!(f.underground.not && cell.is[HiddenTile], "hidden tile")
                }

                ask(f).cancel.needOk

            case DragonMoveAction(f, dir, c, then) =>
                f.powers = f.powers.diff(c)

                if (c.any)
                    f.log("used", c.comma, "to", "Move".styled(f))

                f.position = f.position./(_.add(dir))

                f.path :+= f.position.get

                f.log("moved", dir)

                PromptAttacksAction(f, then)

            case PromptAttacksAction(f, then) =>
                then

            case DragonMoveContinueAction(f, direction) =>
                implicit val ask = builder

                Bearings.wnes.foreach { dir =>
                    val wall = board.wall(f.position.get, dir)

                    val dest = f.position.get.add(dir)

                    val cell = board.get(dest)

                    + DragonMoveAction(f, dir, $, DragonTurnAction(f))
                        .!(dir != direction)
                        .!(f.underground.not && wall, "wall")
                        .!(cell.is[Emptiness.type], "can't move to open space")
                        .!(f.underground.not && cell.is[HiddenTile], "hidden tile")
                }

                ask(f).done(DragonTurnAction(f))

            // REVEAL SCORCH
            case DragonScorchMainAction(f, c) =>
                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Scorch".styled(f))

                DragonRevealAction(f, Around)

            case DragonRevealMainAction(f, c) =>
                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Reveal".styled(f))

                DragonRevealRollAction(f, false)

            case DragonRevealRollAction(f, shriek) =>
                if (shriek) {
                    f.log("used", "Shriek".hl)

                    f.shriek = false
                }

                Random(Pattern.die, DragonRevealRolledAction(f, _))

            case DragonRevealRolledAction(f, x) =>
                f.log("rolled", x, dt.Pattern(x))

                Ask(f)
                    .add(DragonRevealAction(f, x).as("Keep", x)(Image(Flame.img, styles.power, styles.inline) ~ " " ~ Image("pattern-" + x.toString, styles.power, styles.inline)))
                    .when(f.shriek)(DragonRevealRollAction(f, true).as("Reroll"))

            case DragonRevealAction(f, x) =>
                val l = board.pattern(f.position.get, x).%(board.get(_).is[HiddenTile])

                board.writeAll(f, l)

                if (l.none) {
                    f.log("had nothing to reveal")

                    Ask(f)
                        .add(DragonTurnAction(f).as("Slow Burn")(Image(Flame.img, styles.power, styles.inline) ~ " " ~ Image("pattern-" + x.toString, styles.power, styles.inline)))
                        .needOk
                }
                else
                    DragonRevealListAction(f, l, x)

            case DragonRevealListAction(f, l, x) if l.none =>
                DragonTurnAction(f)

            case DragonRevealListAction(f, l, x) =>
                Ask(f).each(l)(p => RevealTileAction(f, p, None, DragonRevealListAction(f, l.but(p), x)).as("Reveal", board.read(f, p).|("?").hl)(Image(Flame.img, styles.power, styles.inline) ~ " " ~ Image("pattern-" + x.toString, styles.power, styles.inline))).needOk

            // BURN
            case DragonBurnMainAction(f, l) =>
                val x = board.inner.%(board.get(_).is[HiddenTile])

                board.writeAll(f, x)

                Ask(f).each(x)(p => DragonBurnAction(f, l, p).as("Reveal", board.read(f, p).|("?").hl)(Image(Flame.img, styles.power, styles.inline) ~ " " ~ Image(Wing.img, styles.power, styles.inline))).needOk

            case DragonBurnAction(f, c, p) =>
                board.writeAll(f, $)

                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Burn".styled(f))

                RevealTileAction(f, p, None, DragonTurnAction(f))

            // ATTACK
            case DragonAttackMainAction(f, c) =>
                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Attack".styled(f))

                DragonAttackRollAction(f, false)

            case DragonAttackRollAction(f, shriek) =>
                if (shriek) {
                    f.log("used", "Shriek".hl)

                    f.shriek = false
                }

                Random(Pattern.die, DragonAttackRolledAction(f, _))

            case DragonAttackRolledAction(f, x) =>
                f.log("rolled", x, dt.Pattern(x))

                Ask(f)
                    .add(DragonAttackAction(f, x).as("Keep", x)(Image(Claw.img, styles.power, styles.inline) ~ " " ~ Image("pattern-" + x.toString, styles.power, styles.inline)))
                    .when(f.shriek)(DragonAttackRollAction(f, true).as("Reroll"))

            case DragonAttackAction(f, x) =>
                val l = board.pattern(f.position.get, x)

                val t : $[AttackTarget] = factions.of[Goblins.type]./~(_.tribes.%(_.position.exists(l.has))./(_.tribe)) ++ factions.of[Knight.type].%(_.position.use(l.has)) ++ factions.of[Thief.type].%(e => e.placed && e.dead.not && l.has(e.position))

                if (t.none)
                    f.log("hit nothing")

                DragonAttackListAction(f, x, t)

            case DragonAttackListAction(f, x, l) if l.none =>
                DragonTurnAction(f)

            case DragonAttackListAction(f, x, l) =>
                Ask(f).each(l)(t => DragonAttackTargetAction(f, t, DragonAttackListAction(f, x, l.but(t))).as("Attack".hh, t)(Image(Claw.img, styles.power, styles.inline) ~ " " ~ Image("pattern-" + x.toString, styles.power, styles.inline))).needOk

            case DragonAttackTargetAction(f, t : Tribe, then) if t.faction.revealed.has(Trap) && t.faction.ignored.has(Trap).not =>
                val e = t.faction
                Ask(e)
                    .add(UseTrapAction(e, then).as("Use", Trap, "to ignore", "Hiss".styled(f), "at", t))
                    .skip(IgnoreTrapAction(e, DragonAttackTargetAction(f, t, then)))

            case DragonAttackTargetAction(f, t : Tribe, then) =>
                f.log("attacked", t)

                f.eat(2 - (t.faction.tribe(t).population <= 1 || t.faction.tribe(t).monsters.has(BrightBeetles)).??(1))

                t.faction.adjustRage(1)

                t.faction.ignored = $

                ScatterTribeAction(t.faction, t, then)

            case DragonAttackTargetAction(f, t : Knight.type, then) =>
                if (t.assigned.has(Shield).not) {
                    f.log("attacked", t)

                    ForceMoveStraightMainAction(f, t, 1, then)
                }
                else {
                    log(Shield, "stopped", f, "attack")

                    then
                }

            case DragonAttackTargetAction(f, t : Thief.type, then) =>
                f.log("attacked", t)
                tryEvasion(t, f, then, killThief(t, then, Some(f)))

            // HISS
            case DragonHissMainAction(f, c) =>
                Ask(f).some(factions.of[Goblins.type])(e => e.tribes.%(_.hidden.not)./(t => DragonHissAction(f, e, t.tribe, c))).cancel

            case DragonHissAction(f, e, t, c) if e.revealed.has(Trap) && e.ignored.has(Trap).not =>
                Ask(e)
                    .add(DragonHissIgnoreAction(f, e, t, c, UseTrapAction(e, DragonTurnAction(f))).as("Use", Trap, "to ignore", "Hiss".styled(f), "at", t))
                    .skip(IgnoreTrapAction(e, ForceAction(DragonHissAction(f, e, t, c))))

            case DragonHissAction(f, e, t, c) =>
                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Hiss".styled(f))

                f.log("hissed at", t)

                e.adjustRage(1)

                t.faction.ignored = $

                ReducePopulationAction(e, t, 1, DragonHissEatAction(f, e, t, e.tribe(t).population))

            case DragonHissIgnoreAction(f, e, t, c, then) =>
                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Hiss".styled(f))

                f.log("hissed at", t)

                then

            case DragonHissEatAction(f, e, t, old) =>
                if (old > e.tribe(t).population)
                    f.eat(old - e.tribe(t).population)

                e.tribe(t).position = None

                DragonTurnAction(f)

            // SHRIEK
            case DragonShriekMainAction(f, c) =>
                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Shriek".styled(f))

                f.log("shrieked")

                f.shriek = true

                DragonTurnAction(f)

            // SLAP
            case DragonSlapMainAction(f, c) =>
                Ask(f).each(factions.of[Knight.type].%(k => Bearings.all.exists(d => f.position.has(k.position.add(d)))).%(_.assigned.has(Shield).not))(k => DragonSlapAction(f, c, k).as("Move", k)("Slap")).cancel.needOk

            case DragonSlapAction(f, c, k) =>
                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Slap".styled(f))

                ForceMoveMainAction(f, k, 3, DragonTurnAction(f))

            // SWAT
            case DragonSwatMainAction(f, c) =>
                Ask(f).some(Bearings.all) { d =>
                    val p = f.position.get.add(d)
                    val h = board.list(p)
                    (h.of[Chest.type] ++ h.of[DragonGem])./(t => DragonSwatAction(f, c, TokenAt(t, p)).as("Swat", t)("Move item"))
                }.cancel.needOk

            case DragonSwatAction(f, c, t) =>
                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Swat".styled(f))

                ForceMoveMainAction(f, t, 5, DragonTurnAction(f))

            // FLAME WALL
            case DragonFlameWallMainAction(f, c) =>
                implicit val ask = builder

                val p = f.position.get

                Bearings.wnes.foreach { dir =>
                    val dest = p.add(dir)

                    + DragonFlameWallAction(f, c, dir)
                        .!(board.get(dest).is[Emptiness.type])
                        .!(board.wall(p, dir))
                }

                ask(f).cancel

            case DragonFlameWallAction(f, c, dir) =>
                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Flame Wall".styled(f))

                board.place(f.position.get.add(dir), FlameWall(f))

                DragonTurnAction(f)

            // SCRATCH
            case DragonScratchMainAction(f, c) =>
                Ask(f).each(factions.but(f).%(_.positions.has(f.position.get)))(t => DragonScratchAction(f, c, t).as(t)("Scratch")).cancel.needOk

            case DragonScratchAction(f, c, k : Knight.type) =>
                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Scratch".styled(f))

                f.log("scratched", k)

                k.score(-5)

                DragonTurnAction(f)

            case DragonScratchAction(f, c, e : Goblins.type) =>
                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Scratch".styled(f))

                f.log("scratched", e)

                if (e.revealed.has(Trap))
                    Ask(e)
                        .add(UseTrapAction(e, DragonTurnAction(f)).as("Use", Trap, "to ignore", "Scratch".styled(f)))
                        .skip(DragonScratchEatAction(f, e))
                else
                    Ask(e).add(DragonScratchEatAction(f, e).as("No Trap"))

            case DragonScratchEatAction(f, e) =>
                e.tribes.%(_.position.has(f.position.get)).foldLeft(DragonTurnAction(f) : ForcedAction)((q, t) => ReducePopulationAction(e, t.tribe, t.population, q))

            // SLITHER
            case DragonSlitherMainAction(f, c) =>
               Ask(f).each(f.tracks)(t => DragonSlitherFromAction(f, c, t.track).as("From", t.track)("Move", "Sloth".styled(styles.sloth)).!(t.value <= 0)).cancel

            case DragonSlitherFromAction(f, c, t) =>
               Ask(f).each(f.tracks)(d => DragonSlitherToAction(f, c, t, d.track).as("To", d.track)("Move", "Sloth".styled(styles.sloth), "from", t).!(d.track == t).!(d.value >= d.track.max)).cancel

            case DragonSlitherToAction(f, c, t, d) =>
                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Slither".styled(f))

                f.tracks.%(_.track == t).foreach { _.value -= 1 }

                f.tracks.%(_.track == d).foreach { _.value += 1 }

                f.log("moved", "Sloth".styled(styles.sloth), "from", t, "to", d)

                DragonTurnAction(f)

            // SMASH
            case DragonSmashMainAction(f, c) =>
                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Smash".styled(f))

                board.remove(f.position.get, Crystal)
                board.place(f.position.get, BrokenCrystal)

                smashed += 1

                f.log("smashed", Crystal)

                DragonTurnAction(f)

            case DragonWrathMainAction(f, c) =>
                f.powers = f.powers.diff(c)

                f.log("used", c.comma, "to", "Wrath".styled(f))

                DragonWrathRollAction(f, false)

            case DragonWrathRollAction(f, shriek) =>
                if (shriek) {
                    f.log("used", "Shriek".hl)

                    f.shriek = false
                }

                Random(Pattern.die, DragonWrathRolledAction(f, _))

            case DragonWrathRolledAction(f, x) =>
                f.log("rolled", x, dt.Pattern(x))

                Ask(f)
                    .add(DragonWrathAction(f, x).as("Keep", x)(4.times(Image("power-any", styles.power, styles.inline) ~ " ") ~ Image("pattern-" + x.toString, styles.power, styles.inline)))
                    .when(f.shriek)(DragonWrathRollAction(f, true).as("Reroll"))

            case DragonWrathAction(f, x) =>
                val l = board.pattern(f.position.get, x).%(board.get(_).is[Emptiness.type].not)

                val positions = $(Knight, Dragon).intersect(factions)./~(_.positions)

                val ll = l.diff(positions) ++ l.intersect(positions)

                val next : ForcedAction = ll.foldRight(ReconnectMapAction(f, true, DragonTurnAction(f)) : ForcedAction)((p, q) => CollapseTileAction(f, p, q, q))

                setup.of[Goblins.type]./~(_.tribes.%(_.position.exists(l.has))).foldLeft(next)((q, t) => ScatterTribeAction(t.faction, t.tribe, q))

            // END OF TURN
            case DragonDoneForfeitAction(f) =>
                Ask(f)
                    .add(DragonEndAction(f).as("End Turn".styled(styles.hit))(f.powers.num.hl, "Power".s(f.powers.num).styled(f), "remaining"))
                    .cancelIf(f.powers.any)

            case DragonEndAction(f) =>
                if (f.path.none && f.prideS.available) {
                    f.prideS.reduce()

                    f.wakefulness += 1

                    f.log("satisfied", "Pride".hl)
                }

                if (f.awake.not && f.wakefulness >= 11) {
                    f.awake = true

                    f.log("has awaken")
                }

                DragonTreasuresAction(f)

            case DragonTreasuresAction(f) =>
                Ask(f).each(board.list(f.position.get).of[Chest.type])(DragonPickTreasureAction(f, _)).done(DragonPlaceGemsAction(f))

            case DragonPickTreasureAction(f, t) =>
                board.remove(f.position.get, t)

                f.treasures += 1

                f.log("took", Chest)

                DragonCheckAction(f, DragonTreasuresAction(f))

            case DragonPlaceGemsAction(f) =>
                implicit def convert(c : Power, selected : Boolean) = {
                    Image("gem-" + c.id, styles.power)
                }

                if (f.gems.num < 3)
                    YYSelectObjectsAction(f, $[Power](Claw, Flame, Wing))
                        .withGroup(f, "places", "Gems".styled(f))
                        .withRuleExcept(f.gems)
                        .withThen(p => DragonPlaceGemAction(f, p))(p => "Place " ~ p.elem ~ " Gem")("")
                        .withExtra($(DragonRedrawAction(f).as("Skip")))
                else
                    DragonRedrawAction(f)

            case DragonPlaceGemAction(f, p) =>
                board.place(f.position.get, DragonGem(f, p))

                f.gems :+= p

                f.log("placed", p, "Gem".hh)

                if (f.gems.num > 1 && f.prideG.available) {
                    f.prideG.reduce()

                    f.wakefulness += 1

                    f.log("satisfied", "Pride".hl)
                }

                DragonRedrawAction(f)

            case DragonRedrawAction(f) =>
                f.powers = $

                f.tracks.foreach(_.reduced = false)

                if (f.awake.not && f.wakefulness >= 11) {
                    f.awake = true

                    f.log("has awaken")
                }

                val then : ForcedAction = DrawPowersAction(f, f.spirit, EndPlayerTurnAction(f))

                if (f.awake && f.underground && board.get(f.position.get).as[Explored].?(_.original.?(_.tokens.has(Crystal)))) {
                    f.underground = false

                    f.log("surfaced")

                    if (board.list(f.position.get).has(Crystal)) {
                        board.remove(f.position.get, Crystal)
                        board.place(f.position.get, BrokenCrystal)

                        smashed += 1

                        f.log("smashed", Crystal)
                    }

                    factions.of[Knight.type].%(_.position == f.position.get).foldLeft(then)((q, k) => ForceAction(AttackAction(k, f, None, false, q)))
                }
                else
                    then

            case DrawPowersAction(f, n, then) =>
                Shuffle(DragonCards.basic, PowersShuffledAction(f, n, _, then))

            case PowersShuffledAction(f, n, l, then) =>
                f.powers ++= l.take(n)

                f.log("drew", n.hl, "power")

                then

    }

}
