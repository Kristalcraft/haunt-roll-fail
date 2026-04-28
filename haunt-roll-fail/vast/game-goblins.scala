// FILE: vast/game-goblins.scala
// VERSION: 0.1.0
// START_MODULE_CONTRACT
// PURPOSE: Goblins `PartialFunction` offload from `Game.performInternalPart2`.
// DEPENDS: vast.Game, Continue, Action; mix into Game only (bytecode offload from monolithic matcher).
// LINKS: M-VAST-GAME
// ROLE: RUNTIME
// END_MODULE_CONTRACT
package vast

import hrf.colmat._
import hrf.elem._
import vast.elem._

trait GameGoblinsSupport { self : Game =>
    protected def goblinDispatchPart2(soft : Void)(implicit g : Game) : PartialFunction[Action, Continue] = {
            // GOBLINS
            case ContinuePlayerTurnAction(f : Goblins.type) =>
                f.effects = $

                f.tribes.foreach { t =>
                    t.activated = false
                    t.halflight = false
                    t.smashing = false
                    t.effects = $
                }

                if (f.rage <= 0) {
                    f.rage = 1

                    f.log("increased", "Rage".styled(styles.hit))
                }

                Shuffle[WarCard](WarCards.all, WarCardsShuffledAction(f, _))

            case WarCardsShuffledAction(f, l) =>
                WarCardsListAction(f, l)

            case WarCardsListAction(f, l) =>
                implicit def convert(c : WarCard) = {
                    Image("war-card-" + c.name, styles.card)
                }

                val s = f.tribes.%(_.position.?(board.get(_) != Emptiness)).some.|(f.tribes)./(_.tribe)

                YYSelectObjectsAction(f, l.take(f.rage))
                    .withGroup(f, "choose", "War Card".styled(f))
                    .withThens { c =>
                        val o = f.tribes.%(t => t.population + c.growth(t.tribe) > 4).%(_.monsters.has(PetFrog).not)./(_.tribe)
                        $(WarCardAction(f, c, o, None).as("Populate", f.tribes./(_.tribe).diff(o).some./(_./(_.elem).commaAnd).|("none".txt), o.any.$("and pay", o.num.hl, "Rage".styled(styles.hit))).!(f.rage + c.rage < o.num)) ++
                        o.any.??(s)./(t => WarCardAction(f, c, $, Some(t)).as("Populate all tribes and scatter", t)) ++
                        (o.any && f.hand.has(Leader)).$(UseSecretAction(f, None, Leader, WarCardAction(f, c, $, None)).as("Avoid overpopulation with", Leader)) ++
                        (f.revealed.has(GoblinRuby) && f.effects.has(GoblinRuby).not).$(RedrawWarCardAction(f, l, c).as("Redraw", c.name.hl)) ++
                        f.hand.has(GoblinRuby).?(RevealSecretAction(f, GoblinRuby, WarCardsListAction(f, l)).as("Reveal", GoblinRuby))
                    }

            case RedrawWarCardAction(f, l, c) =>
                f.log("used", GoblinRuby, "to redraw one", "War Card".styled(f))

                f.effects :+= GoblinRuby

                WarCardsListAction(f, l.but(c).appended(c))

            case WarCardAction(f, c, o, s) =>
                f.log("felt", c)

                f.tribes.foreach { t =>
                    if (o.has(t.tribe).not) {
                        if (t.population < 4 && c.growth(t.tribe) > 0) {
                            t.population = (t.population + c.growth(t.tribe)).clamp(0, 4)
                            log(t.tribe, "population increased to", t.population.hl)
                        }
                    }
                }

                f.adjustRage(c.rage - o.num)

                s.foreach { t =>
                    f.log("scattered", t)
                }

                val then = DrawMonstersAction(f, c.monsters, WarCardSecretsAction(f, c))

                if (s.any)
                    ScatterTribeAction(f, s.get, then)
                else
                    then

           case DrawMonstersAction(f, 0, then) =>
               then

           case DrawMonstersAction(f, n, then) =>
               if (f.monsters.deck.num < n)
                   Shuffle[Monster](f.monsters.pile, TakeMonstersAction(f, n, _, then))
               else
                   Shuffle[Monster]($, TakeMonstersAction(f, n, _, then))

           case TakeMonstersAction(f, n, l, then) =>
               if (l.any) {
                   f.monsters.pile = $
                   f.monsters.deck ++= l
               }

               val mm = f.monsters.deck.take(n)
               f.monsters.deck = f.monsters.deck.drop(n)

               f.log("drew", n, "Monster".s(n).hl)

               AssignMonstersAction(f, mm, then)

           case AssignMonstersAction(f, l, then) if l.none =>
                then

           case AssignMonstersAction(f, l, then) =>
                implicit def convert(c : Monster) = Image("monster-" + c.id, styles.card)

                YYSelectObjectsAction(f, l)
                    .withGroup(f, "assign", "Monsters".styled(f))
                    .withThensInfo { m =>
                        f.tribes./~{ t =>
                            (t.monsters.num < 1 + (t.tribe == Bones).??(1)).$(
                                AddMonsterAction(f, t.tribe, m, AssignMonstersAction(f, l :- m, then))
                                    .as("Add", m, "to", t.tribe)
                                    .!(f.bones.population <= f.tribes./(_.monsters.num).sum)
                            ) ++
                            t.monsters./(o =>
                                ReplaceMonsterAction(f, t.tribe, o, m, AssignMonstersAction(f, l :- m, then))
                                    .as("Replace", o, "with", m, "in", t.tribe)
                                    .!(f.bones.population < f.tribes./(_.monsters.num).sum)
                            )
                        }
                    } {
                        f.tribes./{ t => Info("Add", "Monster".hl, "to", t.tribe) }
                    }
                    .withExtras(then.as("Discard", l./(_.elem).comma))

            case AddMonsterAction(f, t, m, then) =>
                f.log("assigned", m, "to", t)

                f.tribe(t).monsters :+= m

                then

            case ReplaceMonsterAction(f, t, o, m, then) =>
                f.log("removed", o, "from", t)

                f.log("assigned", m, "to", t)

                f.tribe(t).monsters :-= o

                f.monsters.pile :+= o

                f.tribe(t).monsters :+= m

                then

            case WarCardSecretsAction(f, c) =>
                DrawSecretsAction(f, c.secrets, GoblinsTopAction(f))

            case DrawSecretsAction(f, 0, then) =>
                then

            case DrawSecretsAction(f, n, then) =>
                if (f.secrets.deck.num < n)
                    Shuffle[Secret](f.secrets.pile, TakeSecretsAction(f, n, _, then))
                else
                    Shuffle[Secret]($, TakeSecretsAction(f, n, _, then))

            case TakeSecretsAction(f, n, l, then) =>
                if (l.any) {
                    f.secrets.pile = $
                    f.secrets.deck ++= l
                }

                f.log("drew", n, "Secret".s(n).hl)

                f.hand ++= f.secrets.deck.take(n)
                f.secrets.deck = f.secrets.deck.drop(n)

                HandLimitAction(f, n, 5 - f.revealed.num, then)

            case HandLimitAction(f, drawn, n, then) =>
                if (f.hand.num > n)
                    XXSelectObjectsAction(f, f.hand)
                        .withGroup(f.elem ~ " discards " ~ (f.hand.num - n).cards ~ " down to hand limit")
                        .withRule(_.num(f.hand.num - n))
                        .withThen(HandLimitDiscardAction(f, _, then))(l => "Discard".hl ~ l./(" " ~ _.elem))
                        .withExtras(NoHand)
                else
                if (drawn > 0)
                    Ask(f)
                        .each(f.hand.takeRight(drawn))(SecretDrawnInfoAction(f, _))
                        .add(then.as("Ok"))
                        .each(f.hand.dropRight(drawn))(SecretInfoAction(f, _))
                        .needOk
                        .add(NoHand)
                else
                    then

            case HandLimitDiscardAction(f, l, then) =>
                f.hand = f.hand.diff(l)

                f.secrets.pile ++= l

                f.log("discarded", l.num, "Secret".s(l.num).hl, "due to hand limit")

                then

            case GoblinsTopAction(f) =>
                Ask(f).each(f.tribes)(t => {
                    if (t.hidden.not)
                        ActivateTribeAction(f, t.tribe)
                    else
                    if (t.lurking)
                        UnlurkTribeAction(f, t.tribe)
                    else
                        RevealTribeAction(f, t.tribe, GoblinsTopAction(f)).as("Reveal".hh, t.tribe)(f, "activate a Tribe")
                            .!(t.population == 0)
                }.!(t.population <= 0, "unpopulated").!(t.activated, ""))
                .some(f.hand) {
                    case HidingSpots =>
                        factions.of[Knight.type]./~(e => f.tribes.%(_.population > 0).%(_.activated.not).%(_.raw > e.strength).%(_.hidden)./(t =>
                            UseSecretAction(f, |(t.tribe), HidingSpots, GoblinsAttackAction(f, t.tribe, e, DoneTribeAction(f, t.tribe))).as("Use", HidingSpots, "to attack", e, "with", t.tribe)
                        ))
                    case Hex => factions.but(f)./(e => UseSecretAction(f, None, Hex, HexAction(f, e, GoblinsTopAction(f))).as("Use", Hex, "on", e).!(f.eye.population < 1))
                    case CaveIn => $(UseSecretAction(f, None, CaveIn, CaveInMainAction(f, GoblinsTopAction(f))).as("Use", CaveIn).!(f.eye.population < 1))
                    case GoblinRuby => $(RevealSecretAction(f, GoblinRuby, GoblinsTopAction(f)).as("Reveal", GoblinRuby))
                    case Trap => $(RevealSecretAction(f, Trap, GoblinsTopAction(f)).as("Reveal", Trap))
                    case _ => Nil
                }
                .done(EndPlayerTurnAction(f))

            case RevealTribeAction(f, t, then) =>
                var found : $[Relative] = $

                var anywhere = f.tribe(t).monsters.has(Gnome) || (f.tribe(t).hidden.not && f.effects.has(SecretTunnels))

                board.inner.foreach { p =>
                    if (board.get(p).as[HiddenTile].?(_.tribe == t || anywhere))
                        found :+= p
                }

                if (found.none && f.tribe(t).hidden)
                    board.inner.foreach { p =>
                        if (board.get(p) != Emptiness) {
                            Bearings.all.foreach { d =>
                                val e = p.add(d)
                                if (board.get(e) == Emptiness && board.wall(p, d).not) {
                                    found :+= e
                                }
                            }
                        }
                    }

                found = found.distinct

                if (found.none) {
                    f.log("can't reveal", t)

                    then
                }
                else {
                    found = found.sortBy(p => p.x * board.height + p.y)

                    board.writeAll(f, found)

                    Ask(f)
                        .each(found)(p => RevealTribeCellAction(f, t, p).as(board.read(f, p)./("At " ~ _.hl))(f, "reveals", t))
                        .add(RevealTribeCancelAction(f, t, then).as("Cancel"))
                }

           case RevealTribeCancelAction(f, t, then) =>
               board.writeAll(f, $)

               then

           case RevealTribeCellAction(f, t, p) =>
               board.writeAll(f, $)

               f.log("revealed", t)

               f.tribe(t).position = Some(p)

               DoneTribeAction(f, t)

            case UnlurkTribeAction(f, t) =>
                TribeTurnAction(f, t)

            case ActivateTribeAction(f, t) =>
                TribeTurnAction(f, t)

            case TribeTurnAction(f, t) if f.tribe(t).population <= 0 =>
                DoneTribeAction(f, t)

            case TribeTurnAction(f, t) if factions.of[Knight.type].exists(e => f.tribe(t).position.has(e.position)) =>
                Ask(f).each(factions.of[Knight.type].%(e => f.tribe(t).position.has(e.position)))(e => GoblinsAttackAction(f, t, e, DoneTribeAction(f, t)).as("Attack".styled(styles.hit), e).!(e.strength >= f.tribe(t).strength)).bailw(ScatterTribeAction(f, t, DoneTribeAction(f, t))) {
                    log(t, "could not attack")
                }

            case TribeTurnAction(f, t) if factions.of[Thief.type].exists(e => f.tribe(t).position.has(e.position) && e.dead.not) =>
                Ask(f).each(factions.of[Thief.type].%(e => f.tribe(t).position.has(e.position) && e.dead.not))(e => GoblinsAttackAction(f, t, e, DoneTribeAction(f, t)).as("Attack".styled(styles.hit), e)).done(DoneTribeAction(f, t))

            case TribeTurnAction(f, t) => (() => {
                implicit val ask = builder

                val origin = f.tribe(t).position.get

                val unlurk = board.get(origin) == Emptiness

                val reduce = board.get(origin).is[Explored] && f.tribe(t).effects.has(BlindFury).not

                Bearings.wnes.foreach { dir =>
                    val dest = origin.add(dir)

                    val wall = board.wall(origin, dir)

                    val explore = board.get(dest) == Emptiness && tiles.any

                    val move = board.get(dest) != Emptiness

                    val attack = factions.of[Knight.type].%(_.position == dest)
                    val attackThief = factions.of[Thief.type].%(e => e.position == dest && e.dead.not)

                    val burn = board.list(dest).of[FlameWall].any

                    val scatter = f.tribe(t).population <= (reduce && f.tribe(t).halflight).??(1) + burn.??(1)

                    val action =
                        if (unlurk)
                            GoblinsMoveAction(f, t, dir, reduce, DoneTribeAction(f, t))
                        else
                        if (scatter)
                            GoblinsMoveScatterAction(f, t, dir, ScatterTribeAction(f, t, DoneTribeAction(f, t)))
                        else
                        if (attack.any)
                            GoblinsMoveAttackAction(f, t, dir, reduce, GoblinsAttackAction(f, t, attack(0), DoneTribeAction(f, t)))
                        else
                        if (attackThief.any)
                            GoblinsMoveAttackAction(f, t, dir, reduce, GoblinsAttackAction(f, t, attackThief(0), DoneTribeAction(f, t)))
                        else
                        if (explore)
                            GoblinsMoveExploreAction(f, t, dir, reduce, GoblinsExploreAction(f, t, DoneTribeAction(f, t)))
                        else
                            GoblinsMoveAction(f, t, dir, reduce, TribeTurnAction(f, t))

                    + action
                        .!(wall && f.tribe(t).monsters.has(Golem).not, "wall")
                        .!(attack.any && f.tribe(t).strength <= Knight.strength, "not enough strength")
                        .!(unlurk && attack.any)
                        .!(unlurk && attackThief.any)
                        .!(unlurk && explore)
                        .!(explore.not && move.not)
                }

                val scatterFrom = reduce && f.tribe(t).halflight && f.tribe(t).population <= 1

                if (scatterFrom.not && f.tribe(t).monsters.has(Underworm) && f.tribe(t).effects.has(Underworm).not)
                    + UnderwormMainAction(f, t, reduce).as("Ride", Underworm)

                if (f.tribe(t).monsters.has(Wisp) && f.tribe(t).effects.has(Wisp).not)
                    factions.of[Knight.type].%(e => board.visible(origin, e.position)).foreach { e =>
                        + WispMainAction(f, t, e).as(Wisp, e).!(e.assigned.has(Shield), "shield")
                    }

                if (board.list(origin).has(Crystal)) {
                    val complete = f.tribes.exists(o => o.tribe != t && o.smashing && o.position.has(origin))
                    val other = f.tribes.exists(o => o.tribe != t && o.activated.not && o.hidden.not)

                    + HalfSmashCrystalAction(f, t).as(complete.?("Finish").|("Start"), "smashing", Crystal)
                        .!(f.tribe(t).strength < 3, "not enough strength")
                        .!(complete.not && other.not, "need two tribes on the same turn")
                }

                if (board.list(origin).has(Chest))
                    + PlunderChestAction(f, t).as("Plunder", Chest)

                board.list(origin).of[DragonGem].foreach { d =>
                    + PlunderDragonGemAction(f, t, d).as("Plunder", d)
                }

                factions.of[Dragon.type].%(_.position.has(origin)).foreach { d =>
                    + PlunderDragonAction(f, t, d).as("Plunder", d)
                        .!(d.gems.any, "plunder gems instead")
                        .!(f.tribe(t).strength <= d.armor, "not enough strength")
                }

                if (unlurk.not) {
                    if (t == Fangs)
                        + IncreaseRageAction(f, t).as("Increase", "Rage".styled(styles.hit))

                    if (t == Bones)
                        + DrawMonsterAction(f, t).as("Draw", "Monster".hl)

                    if (t == Eye)
                        + DrawSecretAction(f, t).as("Draw", "Secret".hl)
                }

                if (f.hand.has(SecretTunnels)) {
                    + UseSecretAction(f, None, SecretTunnels, TribeTurnAction(f, t)).as("Use", SecretTunnels)
                }

                if (f.effects.has(SecretTunnels)) {
                    + RevealTribeAction(f, t, TribeTurnAction(f, t)).as("Move with", SecretTunnels)
                }

                if (f.hand.has(BlindFury)) {
                    + UseSecretAction(f, |(t), BlindFury, TribeTurnAction(f, t)).as("Use", BlindFury)
                }

                if (f.hand.has(FireBomber)) {
                    + UseSecretAction(f, |(t), FireBomber, TribeTurnAction(f, t)).as("Use", FireBomber)
                }

                + HideTribeAction(f, t).as("Hide".hh)

                if (f.tribe(t).activated)
                    + DoneTribeAction(f, t).as("Done")

                ask(f).needOk.cancelIf(f.tribe(t).activated.not)
            })()

            case GoblinsMoveScatterAction(f, t, dir, then) =>
                Force(GoblinsMoveAction(f, t, dir, true, then))

            case GoblinsMoveAction(f, t, dir, reduce, then) =>
                f.tribe(t).activated = true

                f.tribe(t).position = Some(f.tribe(t).position.get.add(dir))

                log(t, "moved", dir)

                if (board.list(f.tribe(t).position.get).of[FlameWall].any) {
                    log(t, "was burned by", "Flame Wall".styled(Dragon))

                    ReducePopulationAction(f, t, 1, GoblinsMoveLightAction(f, t, reduce, then))
                }
                else
                    GoblinsMoveLightAction(f, t, reduce, then)

            case GoblinsMoveLightAction(f, t, reduce, then) =>
                if (reduce) {
                    f.tribe(t).halflight = f.tribe(t).halflight.not

                    if (f.tribe(t).halflight)
                        then
                    else
                        ReducePopulationAction(f, t, 1, then)
                }
                else
                    then

            case GoblinsMoveExploreAction(f, t, dir, reduce, then) =>
                Force(GoblinsMoveAction(f, t, dir, reduce, then))

            case GoblinsExploreAction(f, t, then) =>
                f.log("explored")

                board.set(f.tribe(t).position.get, Pending)

                board.expand()

                FillOpenEdgesAction(f, $(f.tribe(t).position.get), DoneTribeAction(f, t))

            case GoblinsMoveAttackAction(f, t, dir, reduce, then) =>
                Force(GoblinsMoveAction(f, t, dir, reduce, then))

            case GoblinsAttackAction(f, t, e : Knight.type, then) =>
                if (f.tribe(t).position.has(e.position).not) {
                    f.tribe(t).position = Some(e.position)

                    log(t, "attacked", e)

                    Ask(f).add(GoblinsAttackAction(f, t, e, then).as("Attack".styled(styles.hit))("Target", e))
                }
                else {
                    e.health -= 1

                    e.log("lost", 1.hl, "health")

                    f.adjustRage(-1)

                    if (e.health <= 0) {
                        f.log("killed", e)

                        Milestone(GameOverAction($(f)))
                    }
                    else {
                        if (f.tribe(t).monsters.has(Blob) && f.tribe(t).effects.has(HidingSpots).not)
                            e.score(-5)

                        if (f.tribe(t).effects.has(HidingSpots)) {
                            f.tribe(t).effects :-= HidingSpots

                            f.secrets.pile :+= HidingSpots
                        }

                        val q = ScatterTribeAction(f, t, then)

                        if (f.hand.has(Poison)) {
                            val d = min(e.stamina - 2, (f.tribe(t).strength > e.strength + 1).?(2).|(1))

                            Ask(f).add(ApplyPoisonAction(f, e, d, q).as("Apply", Poison).!(d <= 0, "minimum stamina")).skip(q).needOk
                        }
                        else
                            Ask(f).add(q.as("No Poison"))
                    }
                }

            case GoblinsAttackAction(f, t, e : Thief.type, then) =>
                if (f.tribe(t).position.has(e.position).not) {
                    f.tribe(t).position = Some(e.position)
                    log(t, "attacked", e)
                    Ask(f).add(GoblinsAttackAction(f, t, e, then).as("Attack".styled(styles.hit))("Target", e))
                }
                else {
                    log(t, "attacked", e)
                    tryEvasion(e, f, then, killThief(e, then, Some(f)))
                }

            case ApplyPoisonAction(f, e, d, then) =>
                f.log("applied", Poison)

                f.hand :-= Poison

                f.secrets.pile :+= Poison

                e.poison += d

                f.log("temporary lost", d.hl, Stamina)

                then

            case PlunderChestAction(f, t) =>
                val origin = f.tribe(t).position.get

                board.remove(origin, Chest)

                log(t, "plundered", Chest)

                f.adjustRage(1)

                if (f.tribe(t).monsters.has(Gnome))
                    RevealTribeAction(f, t, DoneTribeAction(f, t))
                else
                    DoneTribeAction(f, t)

            case PlunderDragonGemAction(f, t, d) =>
                val origin = f.tribe(t).position.get

                board.remove(origin, d)

                log(t, "plundered", d)

                d.faction.gems :-= d.power

                Random(Pattern.die, PlunderRollAction(f, t, _))

            case PlunderDragonAction(f, t, d) =>
                val origin = f.tribe(t).position.get

                log(t, "plundered", d)

                Random(Pattern.die, PlunderRollAction(f, t, _))

            case PlunderRollAction(f, t, x) =>
                log(t, "rolled", x, dt.Pattern(x))

                if (x.in(Saltire, Cross, Center)) {
                    log("It's a trap!")

                    ScatterTribeAction(f, t, DoneTribeAction(f, t))
                }
                else {
                    f.adjustRage(1)

                    if (f.tribe(t).monsters.has(Gnome))
                        RevealTribeAction(f, t, DoneTribeAction(f, t))
                    else
                        DoneTribeAction(f, t)
                }

            case HalfSmashCrystalAction(f, t) =>
                val origin = f.tribe(t).position.get

                val other = f.tribes.%(o => o.tribe != t && o.smashing && o.position.has(origin)).single

                if (other.any) {
                    log(t, "finished smashing", Crystal)

                    board.remove(origin, Crystal)
                    board.place(origin, BrokenCrystal)

                    smashed += 1

                    f.log("smashed", Crystal)

                    f.adjustRage(-1)

                    ScatterTribeAction(f, other.get.tribe, ScatterTribeAction(f, t, DoneTribeAction(f, t)))
                }
                else {
                    log(t, "started smashing", Crystal)

                    f.tribe(t).smashing = true

                    DoneTribeAction(f, t)
                }

            case IncreaseRageAction(f, t) =>
                f.adjustRage(1)

                DoneTribeAction(f, t)

            case DrawMonsterAction(f, t) =>
                log(t, "drew", "Monster".hl)

                DrawMonstersAction(f, 1, DoneTribeAction(f, t))

            case DrawSecretAction(f, t) =>
                log(t, "drew", "Secret".hl)

                DrawSecretsAction(f, 1, DoneTribeAction(f, t))

            case HideTribeAction(f, t) =>
                f.tribe(t).position = None

                log(t, "went into hiding")

                DoneTribeAction(f, t)

            case UnderwormMainAction(f, t, reduce) =>
                val p = f.tribe(t).position.get
                val dd = $((-2, -2), (-2, 2), (-1, -1), (-1, 1), (1, -1), (1, 1), (2, -2), (2, 2))
                val l = dd./((dx, dy) => Relative(p.x + dx, p.y + dy))
                val ll = l.%(board.valid).%(p => board.get(p) != Emptiness)

                board.writeAll(f, ll)

                Ask(f)
                    .each(ll)(p =>
                        UnderwormAction(f, t, reduce, p).as("To", board.read(f, p).|("?").hl)(t, "rides", Underworm)
                            .!(factions.of[Knight.type].exists(e => e.position == p && e.strength >= f.tribe(t).strength - (reduce && f.tribe(t).halflight && f.tribe(t).monsters.has(BrightBeetles).not).??(1)), "not enought strength")
                    )
                    .add(UnderwormCancelAction(f, t).as("Cancel"))

            case UnderwormAction(f, t, reduce, p) =>
                board.writeAll(f, $)

                f.tribe(t).activated = true

                f.tribe(t).effects :+= Underworm

                f.tribe(t).position = Some(p)

                log(t, "rode", Underworm)

                if (board.list(f.tribe(t).position.get).of[FlameWall].any) {
                    log(t, "was burned by", "Flame Wall".styled(Dragon))

                    ReducePopulationAction(f, t, 1, GoblinsMoveLightAction(f, t, reduce, TribeTurnAction(f, t)))
                }
                else
                    GoblinsMoveLightAction(f, t, reduce, TribeTurnAction(f, t))

            case UnderwormCancelAction(f, t) =>
                board.writeAll(f, $)

                TribeTurnAction(f, t)

            case WispMainAction(f, t, e) =>
                f.tribe(t).effects :+= Wisp

                log(t, "lured", e, "with", Wisp)

                ForceMoveStraightMainAction(f, e, 3, TribeTurnAction(f, t))

            case UseSecretAction(f, t, s, then) =>
                f.hand :-= s

                f.secrets.pile :+= s

                f.log("used", s)

                t.foreach { t =>
                    f.tribe(t).effects ++= s.as[Effect]
                }

                if (t.none)
                    f.effects ++= s.as[Effect]

                then

            case RevealSecretAction(f, s, then) =>
                f.hand :-= s

                f.revealed :+= s

                f.log("revealed", s)

                then

            case CaveInMainAction(f, then) =>
                val l = board.inner.%(p => board.get(p) != Emptiness).%(p => board.list(p).use(t => t.has(Entrance).not && t.has(Crystal).not)).diff(factions.but(f)./~(_.positions))

                if (l.any) {
                    board.writeAll(f, l)

                    CaveInListAction(f, l, $, then)
                }
                else {
                    f.log("could not remove any tile")

                    then
                }

            case CaveInListAction(f, l, t, then) =>
                t.foreach { p =>
                    if (board.get(p).is[Removing].not)
                        board.set(p, Removing(board.get(p)))
                }

                val n = f.eye.population

                Ask(f)
                    .each(l)(p => CaveInSelectAction(f, l, t, p, then).!(t.has(p)).!(t.num >= n))
                    .when(t.any)(CaveInAction(f, t, then).as("Collapse Tiles"))

            case CaveInSelectAction(f, l, t, p, then) =>
                Ask(f).add(CaveInListAction(f, l, t :+ p, then).as("Test"))

            case CaveInAction(f, t, then) =>
                t.foldLeft(ReconnectMapAction(f, true, then) : ForcedAction)((q, p) => CollapseTileAction(f, p, q, q))

            case HexAction(f, e : Knight.type, then) =>
                f.log("targeted", e)

                e.score(-f.eye.population)

                then

            case HexAction(f, e : Dragon.type, then) =>
                f.log("targeted", e)

                DragonDiscardPowersMainAction(e, f.eye.population, then)

            case HexAction(f, e : Cave.type, then) =>
                f.log("targeted", e)

                CaveDiscardOmensMainAction(e, f.eye.population, then)

            case HexAction(f, e : Thief.type, then) =>
                f.log("targeted", e)
                e.lootDrop = max(0, e.lootDrop - f.eye.population)
                e.log("loot drop reduced to", e.lootDrop.hl, "by Hex")
                then

            case CaveDiscardOmensMainAction(f, n, then) if f.omens.none =>
                Ask(f).add(then.as("No omens to discard"))

            case CaveDiscardOmensMainAction(f, n, then) =>
                implicit def convert(c : OmenCard, selected : Boolean) = selected.?(c.imgs).|(c.img)

                val k = min(n, f.omens.num)

                XXSelectObjectsAction(f, f.omens)
                    .withGroup(f, "discards", k.hlb, "Omen".s(k).styled(f))
                    .withRule(_.num(k))
                    .withThen(l => CaveDiscardOmensAction(f, l, then))(l => "Discard " ~ l./(_.elem).comma)

            case CaveDiscardOmensAction(f, c, then) =>
                f.omens = f.omens.diff(c)

                f.log("discarded", c.comma)

                then

            case DoneTribeAction(f, t) =>
                f.tribe(t).activated = true

                GoblinsTopAction(f)

            case UseTrapAction(f, then) =>
                f.revealed :-= Trap

                f.secrets.pile :+= Trap

                f.log("used", Trap, "to foil the attack")

                then

            case IgnoreTrapAction(f, then) =>
                f.ignored :+= Trap

                then

            case ReducePopulationAction(f, t, n, then) if n <= 0 =>
                then

            case ReducePopulationAction(f, t, n, then)
            if f.tribe(t).monsters.has(BrightBeetles) && f.tribe(t).effects.has(BrightBeetles).not =>
                Ask(f)
                    .add(RemoveMonsterAction(f, t, BrightBeetles, ReducePopulationAction(f, t, n - 1, then)))
                    .add(AddEffectAction(f, t, BrightBeetles, ReducePopulationAction(f, t, n, then)).as("Ignore", BrightBeetles))

            case AddEffectAction(f, t, s, then) =>
                f.tribe(t).effects :+= s

                then

            case ReducePopulationAction(f, t, n, then) =>
                log(t, "population reduced by", n.hl)

                val r = max(0, f.tribe(t).population - n)

                f.tribe(t).population = r

                if (r > 0)
                    then
                else {
                    if (f.revealed.has(GoblinRuby)) {
                        f.log("lost", GoblinRuby)

                        f.revealed :-= GoblinRuby

                        f.secrets.pile :+= GoblinRuby
                    }

                    ScatterTribeAction(f, t, then)
                }

            case ScatterTribeAction(f, t, then) =>
                log(t, "scattered")

                f.tribe(t).position = None

                val n = max(2, f.tribe(t).effects.has(FireBomber).??(f.tribe(t).population)) - f.tribe(t).monsters.count(BrightBeetles)

                val r = max(0, f.tribe(t).population - n)

                log(t, "lost", (f.tribe(t).population - r).hl, "population")

                f.tribe(t).population = r

                if (r == 0)
                    if (f.revealed.has(GoblinRuby)) {
                        f.log("lost", GoblinRuby)

                        f.revealed :-= GoblinRuby

                        f.secrets.pile :+= GoblinRuby
                    }

                if (f.tribe(t).monsters.any)
                    Ask(f).each(f.tribe(t).monsters)(RemoveMonsterAction(f, t, _, then))
                else
                    then

            case RemoveMonsterAction(f, t, m, then) =>
                f.tribe(t).monsters :-= m

                f.monsters.pile :+= m

                log(t, "removed", m)

                then

            case ProcessAmbushAction(f, then) if factions.has(Goblins) =>
                implicit val ask = builder

                + AttackAmbushAction(Goblins, f, then).as("Bring it on!".hh)("Brace for", Ambush)

                assignStamina(f, ProcessAmbushAction(f, then), false, false, true)

                ask(f)

            case AttackAmbushAction(f, k, then) =>
                Ask(f)
                    .each(f.tribes)(t => GoblinsAttackAction(Goblins, t.tribe, k, RemoveAmbushAction(k, then)).as("Ambush".styled(styles.hit), "with", t.tribe)(Goblins, "can", "Ambush".styled(styles.hit)).!(t.hidden.not).!(t.strength <= k.strength))
                    .add(SkipAmbushAction(f, k, then).as("Skip"))

            case SkipAmbushAction(f, k, then) =>
                f.log("didn't", Ambush)

                RemoveAmbushAction(k, then)

            case RemoveAmbushAction(f, then) =>
                val tokens = board.list(f.position)

                board.remove(f.position, Ambush)

                then

    }

}

