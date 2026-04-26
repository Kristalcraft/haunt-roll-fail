// FILE: vast/VastHost.scala
// VERSION: 0.1.0
// START_MODULE_CONTRACT
// PURPOSE: Run bot-driven Vast simulations and act as a host-side verification harness for action flow and serialization stability.
// SCOPE: Resolve Continue branches into actions, drive repeated simulation batches, and capture failure artifacts.
// DEPENDS: vast.Game, vast.Serialize, vast.BotKnight, vast.BotDragon, vast.BotXX
// LINKS: M-VAST-HOST, M-VAST-GAME, M-VAST-BOT, M-VAST-SERIALIZE
// ROLE: SCRIPT
// MAP_MODE: LOCALS
// END_MODULE_CONTRACT
//
// START_MODULE_MAP
// Host - host-side Vast simulation entry point and Continue resolver
// END_MODULE_MAP
//
// START_CHANGE_SUMMARY
// LAST_CHANGE: Renamed from host.scala so JS build does not exclude this file (filter matched all host.scala).
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

object Host {
    private def logMarker(scope : String, block : String, message : String) : Unit = {
        +++("[VastHost][" + scope + "][" + block + "] " + message)
    }

    def askFaction(g : Game, c : Continue) : Action = {
        c match {
            case ErrorContinue(e, _) =>
                error("[VastHost][askFaction][BLOCK_HANDLE_CONTINUE] error=" + e)
                throw e

            case Force(action) =>
                action

            case Log(_, _, continue) =>
                askFaction(g, continue)

            case Milestone(_, action) =>
                action

            case DelayedContinue(_, continue) =>
                askFaction(g, continue)

            case Roll(dice, rolled, tag) =>
                rolled(dice./(_.roll()))

            case Shuffle(list, shuffled, tag) =>
                shuffled(list.shuffle)

            case Shuffle2(l1, l2, shuffled, tag) =>
                shuffled(l1.shuffle, l2.shuffle)

            case Shuffle3(l1, l2, l3, shuffled, tag) =>
                shuffled(l1.shuffle, l2.shuffle, l3.shuffle)

            case ShuffleUntil(list, condition, shuffled, tag) =>
                var r = list.shuffle

                while (!condition(r))
                    r = list.shuffle

                shuffled(r)

            case Random(list, chosen, tag) =>
                chosen(list.shuffle(0))

            case Ask(_, List(action)) =>
                logMarker("askFaction", "BLOCK_RESOLVE_SINGLE_ACTION", "action=" + action.getClass.getSimpleName)
                action

            case Ask(f : Knight.type, actions) =>
                logMarker("askFaction", "BLOCK_SELECT_BOT_ACTION", "faction=" + f.short + " options=" + actions.num)
                new BotKnight(f).ask(actions, 0)(g).immediate

            case Ask(f : Dragon.type, actions) =>
                logMarker("askFaction", "BLOCK_SELECT_BOT_ACTION", "faction=" + f.short + " options=" + actions.num)
                new BotDragon(f).ask(actions, 0)(g).immediate

            case Ask(f : Faction, actions) =>
                logMarker("askFaction", "BLOCK_SELECT_BOT_ACTION", "faction=" + f.short + " options=" + actions.num)
                new BotXX(f).ask(actions, 0)(g).immediate
        }
    }

    // START_CONTRACT: main
    // PURPOSE: Execute repeated host-side Vast simulations and summarize their outcomes.
    // INPUTS: { args: Array[String] - unused CLI arguments for the current simulation harness }
    // OUTPUTS: { Unit - writes summaries and failure artifacts to stdout/files }
    // SIDE_EFFECTS: runs simulations, writes failure files, emits verification log markers
    // LINKS: M-VAST-HOST, V-M-VAST-HOST, V-M-VAST-SERIALIZE
    // END_CONTRACT: main
    def main(args : Array[String]) : Unit = {
        val allFactions : $[Faction] = $(Knight, Goblins, Dragon, Cave)
        val allComb = allFactions.combinations(4).$
        val factions = allFactions
        val repeat = 0.to(15).map(_ => factions)

        var results : $[$[Faction]] = Nil

        val base = repeat

        // START_BLOCK_SIMULATE_GAME
        logMarker("simulate", "BLOCK_SIMULATE_GAME", "factions=" + allFactions./(_.short).mkString(",") + " repetitions=" + repeat.size)
        1.to(20).foreach { i =>
            results = results ++ base/*.par*/.map { ff =>
                var log : $[String] = Nil
                def writeLog(s : String) : Unit = {
                    log = s :: log
                }

                var aa : $[Action] = $

                try {
                    val seating = ff
                    val game = new Game(seating, $)

                    var continue : Continue = StartContinue
                    var a : Action = StartAction(gaming.version)

                    var n = 0
                    while (a.is[GameOverAction].not) {
                        n += 1

//                        /*
                        a match {
                            case a if a.isSoft =>
                            case a : ExternalAction =>
                                try {
                                    +++("[VastSerialize][roundTrip][BLOCK_WRITE_PARSE_ROUNDTRIP] action=" + a.getClass.getSimpleName)
                                    val sss = Serialize.write(a.unwrap)
                                    val ppp = Serialize.parseAction(sss)
                                    val aaa = Serialize.write(ppp)

                                    if (sss != aaa) {
                                        println()
                                        println("UNMATCHING WRITE/PARSE")
                                        println()
                                        println()
                                        println()
                                        println("soft:" + a.isSoft)
                                        println(a)
                                        println()
                                        println(sss)
                                        println()
                                        println(ppp)
                                        println()
                                        println(aaa)
                                        println()
                                        println()
                                        println()
                                    }
                                }
                                catch {
                                    case e =>
                                        println()
                                        println("*")
                                        println("**")
                                        println("*")
                                        println()
                                        println()
                                        println()
                                        println()
                                        println()
                                        println()
                                        println()
                                        println()
                                        println()

                                        println(a)

                                        println()

                                        val sss = Serialize.write(a)

                                        println(sss)

                                        val ppp = Serialize.parseAction(sss)

                                        println(ppp)

                                        val aaa = Serialize.write(ppp)

                                        println(aaa)
                                }

                            case _ =>
                        }
//                        */

                        if (n > 2000)
                            throw null

                        continue = game.performContinue(|(continue), a, true).continue

                        aa :+= a

                        a = askFaction(game, continue)
                    }

                    val w = a.asInstanceOf[GameOverAction].winners
                    println(w.any.?(w./(_.name).mkString(", ")).|("Humanity") + " won (" + n + ")")
                    w
                }
                catch {
                    case e : Throwable if false.not =>
                        error("[VastHost][simulate][BLOCK_SIMULATION_FAILURE] error=" + e)
                        println(e)
                        println(aa./(_.unwrap)./(Serialize.write).mkString("\n"))
                        println(aa./(Serialize.write).mkString("\n"))
                        println(e.getMessage)
                        e.printStackTrace()
                        println(log.reverse.mkString("\n"))
                    Nil
                }
            }

            val wins = results.groupBy(w => w).view.mapValues(_.size).toMap

            println()

            wins.keys.$.sortBy(k => wins(k)).reverse.foreach { k =>
                println(k.any.?(k./(_.name).mkString(", ")).|("Humanity") + ": " + wins(k) + " " + "%6.0f".format(wins(k) * 100.0 / wins.values.sum) + "%")
            }

            println()

            allFactions.map { f =>
                val ww = wins.filterKeys(_.contains(f))
                val solo = ww.filterKeys(_.size == 1).values.sum
                val tie = ww.filterKeys(_.size > 1).values.sum
                (solo + tie) -> (f.name + ": " + solo + "+" + tie + " " + "%6.0f".format((solo + tie) * 100.0 / wins.values.sum) + "%")
            }.sortBy(_._1).map(_._2).reverse.foreach(println)

            println("Humanity" + ": " + wins.filterKeys(_.size == 0).values.sum + " " + "%6.0f".format(wins.filterKeys(_.size == 0).values.sum * 100.0 / wins.values.sum) + "%")
            println("Total: " + results.num)
            println()
            logMarker("simulate", "BLOCK_SIMULATE_GAME", "iteration=" + i + " total=" + results.num)
        }
        // END_BLOCK_SIMULATE_GAME
    }
}
